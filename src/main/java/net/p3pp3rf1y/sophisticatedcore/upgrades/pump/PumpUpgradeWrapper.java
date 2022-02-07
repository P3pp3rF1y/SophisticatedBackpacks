package net.p3pp3rf1y.sophisticatedcore.upgrades.pump;

import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.BucketPickup;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.material.FluidState;
import net.minecraft.world.phys.AABB;
import net.minecraftforge.fluids.FluidAttributes;
import net.minecraftforge.fluids.FluidStack;
import net.minecraftforge.fluids.FluidUtil;
import net.minecraftforge.fluids.IFluidBlock;
import net.minecraftforge.fluids.capability.CapabilityFluidHandler;
import net.minecraftforge.fluids.capability.IFluidHandler;
import net.minecraftforge.fluids.capability.IFluidHandlerItem;
import net.minecraftforge.fluids.capability.wrappers.BucketPickupHandlerWrapper;
import net.minecraftforge.fluids.capability.wrappers.FluidBlockWrapper;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.upgrades.ITickableUpgrade;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeWrapperBase;
import net.p3pp3rf1y.sophisticatedcore.util.NBTHelper;
import net.p3pp3rf1y.sophisticatedcore.util.WorldHelper;

import javax.annotation.Nullable;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Optional;
import java.util.Set;
import java.util.function.Consumer;

public class PumpUpgradeWrapper extends UpgradeWrapperBase<PumpUpgradeWrapper, PumpUpgradeItem> implements ITickableUpgrade {
	private static final int DID_NOTHING_COOLDOWN_TIME = 40;
	private static final int HAND_INTERACTION_COOLDOWN_TIME = 3;
	private static final int WORLD_INTERACTION_COOLDOWN_TIME = 20;
	private static final int FLUID_HANDLER_INTERACTION_COOLDOWN_TIME = 20;
	private static final int PLAYER_SEARCH_RANGE = 3;
	private static final int PUMP_IN_WORLD_RANGE = 4;
	private static final int PUMP_IN_WORLD_RANGE_SQR = PUMP_IN_WORLD_RANGE * PUMP_IN_WORLD_RANGE;

	private long lastHandActionTime = -1;
	private final FluidFilterLogic fluidFilterLogic;
	private final PumpUpgradeConfig pumpUpgradeConfig;

	protected PumpUpgradeWrapper(IStorageWrapper storageWrapper, ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
		super(storageWrapper, upgrade, upgradeSaveHandler);
		pumpUpgradeConfig = upgradeItem.getPumpUpgradeConfig();
		fluidFilterLogic = new FluidFilterLogic(pumpUpgradeConfig.filterSlots.get(), upgrade, upgradeSaveHandler);
	}

	@Override
	public void tick(@Nullable LivingEntity entity, Level world, BlockPos pos) {
		if (isInCooldown(world)) {
			return;
		}
		setCooldown(world, storageWrapper.getFluidHandler().map(storageFluidHandler -> tick(storageFluidHandler, entity, world, pos)).orElse(DID_NOTHING_COOLDOWN_TIME));
	}

	private int tick(IFluidHandlerItem storageFluidHandler, @Nullable LivingEntity entity, Level world, BlockPos pos) {
		if (entity == null) {
			Optional<Integer> newCooldown = handleInWorldInteractions(storageFluidHandler, world, pos);
			if (newCooldown.isPresent()) {
				return newCooldown.get();
			}
		} else {
			if (shouldInteractWithHand() && entity instanceof Player player && handleFluidContainerInHands(player, storageFluidHandler)) {
				lastHandActionTime = world.getGameTime();
				return HAND_INTERACTION_COOLDOWN_TIME;
			}
		}
		return lastHandActionTime + 10 * HAND_INTERACTION_COOLDOWN_TIME > world.getGameTime() ? HAND_INTERACTION_COOLDOWN_TIME : DID_NOTHING_COOLDOWN_TIME;
	}

	private Optional<Integer> handleInWorldInteractions(IFluidHandlerItem storageFluidHandler, Level world, BlockPos pos) {
		if (shouldInteractWithHand() && handleFluidContainersInHandsOfNearbyPlayers(world, pos, storageFluidHandler)) {
			lastHandActionTime = world.getGameTime();
			return Optional.of(HAND_INTERACTION_COOLDOWN_TIME);
		}

		if (shouldInteractWithWorld()) {
			Optional<Integer> newCooldown = interactWithWorld(world, pos, storageFluidHandler);
			if (newCooldown.isPresent()) {
				return newCooldown;
			}
		}

		return interactWithAttachedFluidHandlers(world, pos, storageFluidHandler);
	}

	private Optional<Integer> interactWithAttachedFluidHandlers(Level world, BlockPos pos, IFluidHandler storageFluidHandler) {
		for (Direction dir : Direction.values()) {
			boolean successful = WorldHelper.getBlockEntity(world, pos.offset(dir.getNormal())).map(te ->
					te.getCapability(CapabilityFluidHandler.FLUID_HANDLER_CAPABILITY, dir.getOpposite()).map(fluidHandler -> {
						if (isInput()) {
							return fillFromFluidHandler(fluidHandler, storageFluidHandler, getMaxInOut());
						} else {
							return fillFluidHandler(fluidHandler, storageFluidHandler, getMaxInOut());
						}
					}).orElse(false)).orElse(false);
			if (successful) {
				return Optional.of(FLUID_HANDLER_INTERACTION_COOLDOWN_TIME);
			}
		}

		return Optional.empty();
	}

	private int getMaxInOut() {
		return Math.max(FluidAttributes.BUCKET_VOLUME, pumpUpgradeConfig.maxInputOutput.get() * storageWrapper.getNumberOfSlotRows() * getAdjustedStackMultiplier(storageWrapper));
	}

	public int getAdjustedStackMultiplier(IStorageWrapper storageWrapper) {
		return 1 + (int) (pumpUpgradeConfig.stackMultiplierRatio.get() * (storageWrapper.getInventoryHandler().getStackSizeMultiplier() - 1));
	}

	private Optional<Integer> interactWithWorld(Level world, BlockPos pos, IFluidHandler storageFluidHandler) {
		if (isInput()) {
			return fillFromBlockInRange(world, pos, storageFluidHandler);
		} else {
			for (Direction dir : Direction.values()) {
				BlockPos offsetPos = pos.offset(dir.getNormal());
				if (placeFluidInWorld(world, storageFluidHandler, dir, offsetPos)) {
					return Optional.of(WORLD_INTERACTION_COOLDOWN_TIME);
				}
			}
		}
		return Optional.empty();
	}

	private boolean placeFluidInWorld(Level world, IFluidHandler storageFluidHandler, Direction dir, BlockPos offsetPos) {
		if (dir != Direction.UP) {
			for (int tank = 0; tank < storageFluidHandler.getTanks(); tank++) {
				FluidStack tankFluid = storageFluidHandler.getFluidInTank(tank);
				if (!tankFluid.isEmpty() && fluidFilterLogic.fluidMatches(tankFluid.getFluid())
						&& isValidForFluidPlacement(world, offsetPos) && FluidUtil.tryPlaceFluid(null, world, InteractionHand.MAIN_HAND, offsetPos, storageFluidHandler, tankFluid)) {
					return true;
				}
			}
		}
		return false;
	}

	private boolean isValidForFluidPlacement(Level world, BlockPos offsetPos) {
		BlockState blockState = world.getBlockState(offsetPos);
		return blockState.isAir() || (!blockState.getFluidState().isEmpty() && !blockState.getFluidState().isSource());
	}

	private Optional<Integer> fillFromBlockInRange(Level world, BlockPos basePos, IFluidHandler storageFluidHandler) {
		LinkedList<BlockPos> nextPositions = new LinkedList<>();
		Set<BlockPos> searchedPositions = new HashSet<>();
		nextPositions.add(basePos);

		while (!nextPositions.isEmpty()) {
			BlockPos pos = nextPositions.poll();
			if (fillFromBlock(world, pos, storageFluidHandler)) {
				return Optional.of((int) (Math.max(1, Math.sqrt(basePos.distSqr(pos))) * WORLD_INTERACTION_COOLDOWN_TIME));
			}

			for (Direction dir : Direction.values()) {
				BlockPos offsetPos = pos.offset(dir.getNormal());
				if (!searchedPositions.contains(offsetPos)) {
					searchedPositions.add(offsetPos);
					if (basePos.distSqr(offsetPos) < PUMP_IN_WORLD_RANGE_SQR) {
						nextPositions.add(offsetPos);
					}
				}
			}
		}
		return Optional.empty();
	}

	private boolean fillFromBlock(Level world, BlockPos pos, IFluidHandler storageFluidHandler) {
		FluidState fluidState = world.getFluidState(pos);
		if (!fluidState.isEmpty()) {
			BlockState state = world.getBlockState(pos);
			Block block = state.getBlock();
			IFluidHandler targetFluidHandler;
			if (block instanceof IFluidBlock fluidBlock) {
				targetFluidHandler = new FluidBlockWrapper(fluidBlock, world, pos);
			} else if (block instanceof BucketPickup bucketPickup) {
				targetFluidHandler = new BucketPickupHandlerWrapper(bucketPickup, world, pos);
			} else {
				return false;
			}
			return fillFromFluidHandler(targetFluidHandler, storageFluidHandler);
		}
		return false;
	}

	private boolean handleFluidContainersInHandsOfNearbyPlayers(Level world, BlockPos pos, IFluidHandler storageFluidHandler) {
		AABB searchBox = new AABB(pos).inflate(PLAYER_SEARCH_RANGE);
		for (Player player : world.players()) {
			if (searchBox.contains(player.getX(), player.getY(), player.getZ()) && handleFluidContainerInHands(player, storageFluidHandler)) {
				return true;
			}
		}
		return false;
	}

	private boolean handleFluidContainerInHands(Player player, IFluidHandler storageFluidHandler) {
		return handleFluidContainerInHand(storageFluidHandler, player, InteractionHand.MAIN_HAND) || handleFluidContainerInHand(storageFluidHandler, player, InteractionHand.OFF_HAND);
	}

	private boolean handleFluidContainerInHand(IFluidHandler storageFluidHandler, Player player, InteractionHand hand) {
		ItemStack itemInHand = player.getItemInHand(hand);
		if (itemInHand.getCount() > 1 || itemInHand == storageWrapper.getWrappedStorageStack()) {
			return false;
		}
		return itemInHand.getCapability(CapabilityFluidHandler.FLUID_HANDLER_ITEM_CAPABILITY).map(itemFluidHandler -> {
			if (isInput()) {
				return fillFromHand(player, hand, itemFluidHandler, storageFluidHandler);
			} else {
				return fillContainerInHand(player, hand, itemFluidHandler, storageFluidHandler);
			}
		}).orElse(false);
	}

	private boolean fillContainerInHand(Player player, InteractionHand hand, IFluidHandlerItem itemFluidHandler, IFluidHandler storageFluidHandler) {
		boolean ret = fillFluidHandler(itemFluidHandler, storageFluidHandler);
		if (ret) {
			player.setItemInHand(hand, itemFluidHandler.getContainer());
		}
		return ret;
	}

	private boolean fillFluidHandler(IFluidHandler fluidHandler, IFluidHandler storageFluidHandler) {
		return fillFluidHandler(fluidHandler, storageFluidHandler, FluidAttributes.BUCKET_VOLUME);
	}

	private boolean fillFluidHandler(IFluidHandler fluidHandler, IFluidHandler storageFluidHandler, int maxFill) {
		boolean ret = false;
		for (int tank = 0; tank < storageFluidHandler.getTanks(); tank++) {
			FluidStack tankFluid = storageFluidHandler.getFluidInTank(tank);
			if (!tankFluid.isEmpty() && fluidFilterLogic.fluidMatches(tankFluid.getFluid())
					&& !FluidUtil.tryFluidTransfer(fluidHandler, storageFluidHandler, new FluidStack(tankFluid.getFluid(), maxFill), true).isEmpty()) {
				ret = true;
				break;
			}
		}
		return ret;
	}

	private boolean fillFromHand(Player player, InteractionHand hand, IFluidHandlerItem itemFluidHandler, IFluidHandler storageFluidHandler) {
		if (fillFromFluidHandler(itemFluidHandler, storageFluidHandler)) {
			player.setItemInHand(hand, itemFluidHandler.getContainer());
			return true;
		}
		return false;
	}

	private boolean fillFromFluidHandler(IFluidHandler fluidHandler, IFluidHandler storageFluidHandler) {
		return fillFromFluidHandler(fluidHandler, storageFluidHandler, FluidAttributes.BUCKET_VOLUME);
	}

	private boolean fillFromFluidHandler(IFluidHandler fluidHandler, IFluidHandler storageFluidHandler, int maxDrain) {
		FluidStack containedFluid = fluidHandler.drain(maxDrain, IFluidHandler.FluidAction.SIMULATE);
		if (!containedFluid.isEmpty() && fluidFilterLogic.fluidMatches(containedFluid.getFluid())) {
			return !FluidUtil.tryFluidTransfer(storageFluidHandler, fluidHandler, containedFluid, true).isEmpty();
		}
		return false;
	}

	public void setIsInput(boolean input) {
		NBTHelper.setBoolean(upgrade, "input", input);
		save();
	}

	public boolean isInput() {
		return NBTHelper.getBoolean(upgrade, "input").orElse(true);
	}

	public FluidFilterLogic getFluidFilterLogic() {
		return fluidFilterLogic;
	}

	public void setInteractWithHand(boolean interactWithHand) {
		NBTHelper.setBoolean(upgrade, "interactWithHand", interactWithHand);
		save();
	}

	public boolean shouldInteractWithHand() {
		return NBTHelper.getBoolean(upgrade, "interactWithHand").orElse(upgradeItem.getInteractWithHandDefault());
	}

	public void setInteractWithWorld(boolean interactWithWorld) {
		NBTHelper.setBoolean(upgrade, "interactWithWorld", interactWithWorld);
		save();
	}

	public boolean shouldInteractWithWorld() {
		return NBTHelper.getBoolean(upgrade, "interactWithWorld").orElse(upgradeItem.getInteractWithWorldDefault());
	}
}
