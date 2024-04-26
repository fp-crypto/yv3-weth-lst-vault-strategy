// SPDX-License-Identifier: AGPL-3.0
pragma solidity 0.8.18;

import {BaseHealthCheck} from "@periphery/Bases/HealthCheck/BaseHealthCheck.sol";
import {ERC20} from "@tokenized-strategy/BaseStrategy.sol";

import {Math} from "@openzeppelin/contracts/utils/math/Math.sol";
import {SafeERC20} from "@openzeppelin/contracts/token/ERC20/utils/SafeERC20.sol";
import {IERC4626} from "@openzeppelin/contracts/interfaces/IERC4626.sol";

import {IUniswapV3Pool} from "@uniswap-v3-core/interfaces/IUniswapV3Pool.sol";
import {IUniswapV3SwapCallback} from "@uniswap-v3-core/interfaces/callback/IUniswapV3SwapCallback.sol";
import {IUniswapV3Factory} from "@uniswap-v3-core/interfaces/IUniswapV3Factory.sol";

import {IChainlinkAggregator} from "./interfaces/chainlink/IChainlinkAggregator.sol";

contract Strategy is BaseHealthCheck, IUniswapV3SwapCallback {
    using SafeERC20 for ERC20;

    address internal constant WETH = 0xC02aaA39b223FE8D0A0e5C4F27eAD9083C756Cc2;
    IUniswapV3Factory internal constant UNISWAP_FACTORY =
        IUniswapV3Factory(0x1F98431c8aD98523631AE4a59f267346ea31F984);

    uint256 internal constant WAD = 1e18;

    /// @dev The minimum value that can be returned from #getSqrtRatioAtTick. Equivalent to getSqrtRatioAtTick(MIN_TICK)
    uint160 internal constant UNISWAP_MIN_SQRT_RATIO = 4295128739;
    /// @dev The maximum value that can be returned from #getSqrtRatioAtTick. Equivalent to getSqrtRatioAtTick(MAX_TICK)
    uint160 internal constant UNISWAP_MAX_SQRT_RATIO =
        1461446703485210103287273052203988822378723970342;

    ERC20 public immutable lst;
    IERC4626 public immutable lstVault;
    bool private immutable uniswapWeth0Lst1;
    IChainlinkAggregator public immutable chainlinkOracle;
    bool public immutable oracleWrapped;
    bytes4 public immutable unwrappedToWrappedSelector;

    uint16 public slippageAllowedBps = 50; // 0.50%
    uint16 public maxTendBasefeeGwei = 30; // 30 gwei
    IUniswapV3Pool public uniswapPool;
    uint256 public depositLimit;

    constructor(
        string memory _name,
        address _lst,
        address _lstVault,
        uint24 _uniswapFee,
        bytes4 _unwrappedToWrappedSelector,
        address _chainlinkOracle,
        bool _oracleWrapped
    ) BaseHealthCheck(WETH, _name) {
        require(_lst == IERC4626(_lstVault).asset());
        lst = ERC20(_lst);
        lstVault = IERC4626(_lstVault);
        uniswapWeth0Lst1 = WETH < _lst;

        unwrappedToWrappedSelector = _unwrappedToWrappedSelector;
        chainlinkOracle = IChainlinkAggregator(_chainlinkOracle);
        oracleWrapped = _oracleWrapped;

        _setUniswapFee(_uniswapFee);
        ERC20(_lst).safeApprove(_lstVault, type(uint256).max);
    }

    /*******************************************
     *          PUBLIC VIEW FUNCTIONS          *
     *******************************************/

    /**
     * @notice A conservative estimate of assets taking into account
     * the max slippage allowed
     *
     * @return . estimated total assets
     */
    function estimatedTotalAssets() public view returns (uint256) {
        return
            _calculateLstValue(
                lst.balanceOf(address(this)) +
                    lstVault.convertToAssets(lstVault.balanceOf(address(this))),
                _getLstPerWeth()
            ) + _looseAssets();
    }

    /**
     * @notice A liberal estimate of assets not taking into account
     * the max slippage allowed
     *
     * @return . estimated total assets
     */
    function estimatedTotalAssetsNoSlippage() public view returns (uint256) {
        return
            _calculateLstValue(
                lst.balanceOf(address(this)) +
                    lstVault.convertToAssets(lstVault.balanceOf(address(this))),
                _getLstPerWeth()
            ) + _looseAssets();
    }

    /*******************************************
     *          MANAGEMENT FUNCTIONS           *
     *******************************************/

    /**
     * @notice Sets the uniswap fee tier. Can only be called by management
     * @param _fee The uniswap fee tier to use for Asset<->Weth swaps
     */
    function setUniswapFee(uint24 _fee) external onlyManagement {
        _setUniswapFee(_fee);
    }

    /**
     * @notice Sets the deposit limit. Can only be called by management
     * @param _depositLimit The deposit limit
     */
    function setDepositLimit(uint256 _depositLimit) external onlyManagement {
        depositLimit = _depositLimit;
    }

    /**
     * @notice Sets the slippage allowed on swaps. Can only be called by management
     * @param _slippageAllowedBps The slippage allowed in basis points
     */
    function setSlippageAllowedBps(
        uint16 _slippageAllowedBps
    ) external onlyManagement {
        require(_slippageAllowedBps <= MAX_BPS); // dev: cannot be more than 100%
        slippageAllowedBps = _slippageAllowedBps;
    }

    /**
     * @notice Sets the max base fee for tends. Can only be called by management
     * @param _maxTendBasefeeGwei The maximum base fee allowed in gwei
     */
    function setMaxTendBasefeeGwei(
        uint16 _maxTendBasefeeGwei
    ) external onlyManagement {
        maxTendBasefeeGwei = _maxTendBasefeeGwei;
    }

    /*//////////////////////////////////////////////////////////////
                NEEDED TO BE OVERRIDDEN BY STRATEGIST
    //////////////////////////////////////////////////////////////*/

    /**
     * @dev Can deploy up to '_amount' of 'asset' in the yield source.
     *
     * This function is called at the end of a {deposit} or {mint}
     * call. Meaning that unless a whitelist is implemented it will
     * be entirely permissionless and thus can be sandwiched or otherwise
     * manipulated.
     *
     * @param _amount The amount of 'asset' that the strategy can attempt
     * to deposit in the yield source.
     */
    function _deployFunds(uint256 _amount) internal override {
        // do nothing
    }

    /**
     * @dev Should attempt to free the '_amount' of 'asset'.
     *
     * NOTE: The amount of 'asset' that is already loose has already
     * been accounted for.
     *
     * This function is called during {withdraw} and {redeem} calls.
     * Meaning that unless a whitelist is implemented it will be
     * entirely permissionless and thus can be sandwiched or otherwise
     * manipulated.
     *
     * Should not rely on asset.balanceOf(address(this)) calls other than
     * for diff accounting purposes.
     *
     * Any difference between `_amount` and what is actually freed will be
     * counted as a loss and passed on to the withdrawer. This means
     * care should be taken in times of illiquidity. It may be better to revert
     * if withdraws are simply illiquid so not to realize incorrect losses.
     *
     * @param _amount, The amount of 'asset' to be freed.
     */
    function _freeFunds(uint256 _amount) internal override {
        // 1. scale amount
        // 2. calculate mix that needs to be withdrawn/swapped
        uint256 _lstToSwap = (_amount * _getLstPerWeth()) / WAD;
        uint256 _lstLoose = _looseLst();
        if (_lstToSwap > _lstLoose) {
            _lstLoose += lstVault.withdraw(
                _lstToSwap - _lstLoose,
                address(this),
                address(this)
            );
        }
        // 3. Swap lst to wth
        _swapToWeth(Math.min(_lstToSwap, _lstLoose));
    }

    /**
     * @dev Internal function to harvest all rewards, redeploy any idle
     * funds and return an accurate accounting of all funds currently
     * held by the Strategy.
     *
     * This should do any needed harvesting, rewards selling, accrual,
     * redepositing etc. to get the most accurate view of current assets.
     *
     * NOTE: All applicable assets including loose assets should be
     * accounted for in this function.
     *
     * Care should be taken when relying on oracles or swap values rather
     * than actual amounts as all Strategy profit/loss accounting will
     * be done based on this returned value.
     *
     * This can still be called post a shutdown, a strategist can check
     * `TokenizedStrategy.isShutdown()` to decide if funds should be
     * redeployed or simply realize any profits/losses.
     *
     * @return _totalAssets A trusted and accurate account for the total
     * amount of 'asset' the strategy currently holds including idle funds.
     */
    function _harvestAndReport()
        internal
        override
        returns (uint256 _totalAssets)
    {
        adjustPosition(asset.balanceOf(address(this)));
        _totalAssets = estimatedTotalAssets();
    }

    /*//////////////////////////////////////////////////////////////
                    OPTIONAL TO OVERRIDE BY STRATEGIST
    //////////////////////////////////////////////////////////////*/

    /**
     * @notice Gets the max amount of `asset` that can be withdrawn.
     * @dev Defaults to an unlimited amount for any address. But can
     * be overridden by strategists.
     *
     * This function will be called before any withdraw or redeem to enforce
     * any limits desired by the strategist. This can be used for illiquid
     * or sandwichable strategies.
     *
     *   EX:
     *       return asset.balanceOf(yieldSource);
     *
     * This does not need to take into account the `_owner`'s share balance
     * or conversion rates from shares to assets.
     *
     * @param . The address that is withdrawing from the strategy.
     * @return . The available amount that can be withdrawn in terms of `asset`
     */
    function availableWithdrawLimit(
        address /*_owner*/
    ) public view override returns (uint256) {
        // NOTE: Withdraw limitations such as liquidity constraints should be accounted for HERE
        //  rather than _freeFunds in order to not count them as losses on withdraws.

        // TODO: If desired implement withdraw limit logic and any needed state variables.

        // EX:
        // if(yieldSource.notShutdown()) {
        //    return asset.balanceOf(address(this)) + asset.balanceOf(yieldSource);
        // }
        return asset.balanceOf(address(this));
    }

    /**
     * @notice Gets the max amount of `asset` that an address can deposit.
     * @dev Defaults to an unlimited amount for any address. But can
     * be overridden by strategists.
     *
     * This function will be called before any deposit or mints to enforce
     * any limits desired by the strategist. This can be used for either a
     * traditional deposit limit or for implementing a whitelist etc.
     *
     *   EX:
     *      if(isAllowed[_owner]) return super.availableDepositLimit(_owner);
     *
     * This does not need to take into account any conversion rates
     * from shares to assets. But should know that any non max uint256
     * amounts may be converted to shares. So it is recommended to keep
     * custom amounts low enough as not to cause overflow when multiplied
     * by `totalSupply`.
     *
     * @param . The address that is depositing into the strategy.
     * @return . The available amount the `_owner` can deposit in terms of `asset`
     *
     */
    function availableDepositLimit(
        address /*_owner */
    ) public view override returns (uint256) {
        uint256 _totalAssets = TokenizedStrategy.totalAssets();
        return _totalAssets >= depositLimit ? 0 : depositLimit - _totalAssets;
    }

    /**
     * @dev Optional function for strategist to override that can
     *  be called in between reports.
     *
     * If '_tend' is used tendTrigger() will also need to be overridden.
     *
     * This call can only be called by a permissioned role so may be
     * through protected relays.
     *
     * This can be used to harvest and compound rewards, deposit idle funds,
     * perform needed position maintenance or anything else that doesn't need
     * a full report for.
     *
     *   EX: A strategy that can not deposit funds without getting
     *       sandwiched can use the tend when a certain threshold
     *       of idle to totalAssets has been reached.
     *
     * This will have no effect on PPS of the strategy till report() is called.
     *
     * @param _totalIdle The current amount of idle funds that are available to deploy.
     *
     */
    function _tend(uint256 _totalIdle) internal override {
        adjustPosition(_totalIdle);
    }

    /**
     * @dev Optional trigger to override if tend() will be used by the strategy.
     * This must be implemented if the strategy hopes to invoke _tend().
     *
     * @return . Should return true if tend() should be called by keeper or false if not.
     *
    function _tendTrigger() internal view override returns (bool) {}
    */

    /**
     * @dev Optional function for a strategist to override that will
     * allow management to manually withdraw deployed funds from the
     * yield source if a strategy is shutdown.
     *
     * This should attempt to free `_amount`, noting that `_amount` may
     * be more than is currently deployed.
     *
     * NOTE: This will not realize any profits or losses. A separate
     * {report} will be needed in order to record any profit/loss. If
     * a report may need to be called after a shutdown it is important
     * to check if the strategy is shutdown during {_harvestAndReport}
     * so that it does not simply re-deploy all funds that had been freed.
     *
     * EX:
     *   if(freeAsset > 0 && !TokenizedStrategy.isShutdown()) {
     *       depositFunds...
     *    }
     *
     * @param _amount The amount of asset to attempt to free.
     *
     */
    function _emergencyWithdraw(uint256 _amount) internal override {
        _freeFunds(_amount); // TODO: more?
    }

    /**************************************************
     *               INTERNAL ACTIONS                 *
     **************************************************/

    function adjustPosition(uint256 _looseAsset) internal {
        // 1. convert asset to lst
        _swapToLst(_looseAsset);
        // 2. deposit lst in lstVaule
        uint256 _lstAmount = _looseLst();
        lstVault.deposit(
            Math.min(_lstAmount, lstVault.maxDeposit(address(this))),
            address(this)
        );
    }

    /**************************************************
     *               UNISWAP FUNCTIONS                *
     **************************************************/

    function _swapToLst(uint256 _wethAmount) internal returns (uint256) {
        return _swap(_wethAmount, uniswapWeth0Lst1);
    }

    function _swapToWeth(uint256 _lstAmount) internal returns (uint256) {
        return _swap(_lstAmount, !uniswapWeth0Lst1);
    }

    function _swap(
        uint256 _amount,
        bool _zeroForOne
    ) internal returns (uint256) {
        (int256 amount0Delta, int256 amount1Delta) = uniswapPool.swap(
            address(this),
            _zeroForOne,
            -int256(_amount),
            (
                _zeroForOne
                    ? UNISWAP_MIN_SQRT_RATIO + 1
                    : UNISWAP_MAX_SQRT_RATIO - 1
            ),
            ""
        );

        return uint256(_zeroForOne ? amount1Delta : amount0Delta);
    }

    /// @inheritdoc IUniswapV3SwapCallback
    function uniswapV3SwapCallback(
        int256 _amount0Delta,
        int256 _amount1Delta,
        bytes calldata /* _data */
    ) external {
        require(msg.sender == address(uniswapPool)); // dev: callback only called by pool
        require(_amount0Delta > 0 || _amount1Delta > 0); // dev: swaps entirely within 0-liquidity regions are not supported

        (
            ERC20 _inputToken,
            uint256 _amountToPay,
            uint256 _amountReceived
        ) = _amount0Delta > 0
                ? (
                    uniswapWeth0Lst1 ? ERC20(WETH) : lst,
                    uint256(_amount0Delta),
                    uint256(-_amount1Delta)
                )
                : (
                    !uniswapWeth0Lst1 ? ERC20(WETH) : lst,
                    uint256(_amount1Delta),
                    uint256(-_amount0Delta)
                );
        _inputToken.transfer(msg.sender, _amountToPay);
    }

    /**************************************************
     *               INTERNAL VIEWS                   *
     **************************************************/

    /**
     *  @notice Returns the strategy assets which are held as loose asset
     *  @return . The strategy's loose asset
     */
    function _looseAssets() internal view returns (uint256) {
        return asset.balanceOf(address(this));
    }

    /**
     *  @notice Returns the strategy lst which are held as loose lst
     *  @return . The strategy's loose lst
     */
    function _looseLst() internal view returns (uint256) {
        return lst.balanceOf(address(this));
    }

    /**
     *  @notice Retrieves the oracle rate asset/quoteToken
     *  @return Conversion rate
     */
    function _getLstPerWeth() internal view returns (uint256) {
        uint256 _answer = (WAD ** 2) / uint256(chainlinkOracle.latestAnswer());
        if (oracleWrapped) {
            return _answer;
        }
        return _unwrappedToWrappedAsset(_answer);
    }

    function _unwrappedToWrappedAsset(
        uint256 _amount
    ) internal view returns (uint256) {
        (bool success, bytes memory data) = address(asset).staticcall(
            abi.encodeWithSelector(unwrappedToWrappedSelector, _amount)
        );
        require(success, "!success"); // dev: static call failed
        return abi.decode(data, (uint256));
    }

    /**************************************************
     *               INTERNAL SETTERS                 *
     **************************************************/

    function _setUniswapFee(uint24 _fee) internal {
        IUniswapV3Pool _uniswapPool = IUniswapV3Pool(
            UNISWAP_FACTORY.getPool(WETH, address(lst), _fee)
        );
        require(
            _uniswapPool.token0() == address(WETH) ||
                _uniswapPool.token1() == address(WETH)
        ); // dev: pool must contain weth
        require(
            _uniswapPool.token0() == address(lst) ||
                _uniswapPool.token1() == address(lst)
        ); // dev: pool must contain weth
        uniswapPool = _uniswapPool;
    }

    /**************************************************
     *                INTERNAL HELPERS                *
     **************************************************/

    function _calculateLstValueWithMaxSlippage(
        uint256 _lstAmount,
        uint256 _lstPerWeth
    ) internal pure returns (uint256) {
        return _calculateLstValue(_lstAmount, _lstPerWeth);
    }

    function _calculateLstValue(
        uint256 _lstAmount,
        uint256 _lstPerWeth
    ) internal pure returns (uint256) {
        if (_lstAmount == 0 || _lstPerWeth == 0) return 0;

        return (_lstAmount * WAD) / _lstPerWeth;
    }
}
