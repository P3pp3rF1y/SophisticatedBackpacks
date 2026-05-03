package net.p3pp3rf1y.sophisticatedbackpacks.backpack;

import com.mojang.math.Axis;
import net.minecraft.ChatFormatting;
import net.minecraft.client.Minecraft;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.sounds.SoundSource;
import net.minecraft.util.RandomSource;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.ItemInteractionResult;
import net.minecraft.world.SimpleMenuProvider;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.item.ItemEntity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.BlockGetter;
import net.minecraft.world.level.Explosion;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.LevelAccessor;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.EntityBlock;
import net.minecraft.world.level.block.SimpleWaterloggedBlock;
import net.minecraft.world.level.block.SoundType;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.entity.BlockEntityTicker;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.StateDefinition;
import net.minecraft.world.level.block.state.properties.BlockStateProperties;
import net.minecraft.world.level.block.state.properties.BooleanProperty;
import net.minecraft.world.level.block.state.properties.DirectionProperty;
import net.minecraft.world.level.gameevent.GameEvent;
import net.minecraft.world.level.material.FluidState;
import net.minecraft.world.level.material.Fluids;
import net.minecraft.world.level.material.MapColor;
import net.minecraft.world.level.material.PushReaction;
import net.minecraft.world.phys.BlockHitResult;
import net.minecraft.world.phys.Vec3;
import net.minecraft.world.phys.shapes.CollisionContext;
import net.minecraft.world.phys.shapes.VoxelShape;
import net.neoforged.neoforge.capabilities.Capabilities;
import net.neoforged.neoforge.event.entity.player.PlayerInteractEvent;
import net.neoforged.neoforge.fluids.FluidActionResult;
import net.neoforged.neoforge.fluids.FluidType;
import net.neoforged.neoforge.fluids.FluidUtil;
import net.neoforged.neoforge.fluids.capability.IFluidHandlerItem;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.SBPTranslationHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContext;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModBlocks;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.everlasting.EverlastingUpgradeItem;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.api.IUpgradeRenderer;
import net.p3pp3rf1y.sophisticatedcore.client.render.UpgradeRenderRegistry;
import net.p3pp3rf1y.sophisticatedcore.controller.IControllableStorage;
import net.p3pp3rf1y.sophisticatedcore.renderdata.IUpgradeRenderData;
import net.p3pp3rf1y.sophisticatedcore.renderdata.RenderInfo;
import net.p3pp3rf1y.sophisticatedcore.renderdata.UpgradeRenderDataType;
import net.p3pp3rf1y.sophisticatedcore.upgrades.infinity.InfinityUpgradeItem;
import net.p3pp3rf1y.sophisticatedcore.upgrades.jukebox.ServerStorageSoundHandler;
import net.p3pp3rf1y.sophisticatedcore.util.CapabilityHelper;
import net.p3pp3rf1y.sophisticatedcore.util.InventoryHelper;
import net.p3pp3rf1y.sophisticatedcore.util.WorldHelper;
import org.joml.Vector3f;

import javax.annotation.Nullable;

import static net.minecraft.world.level.block.state.properties.BlockStateProperties.WATERLOGGED;

public class BackpackBlock extends Block implements EntityBlock, SimpleWaterloggedBlock {
	public static final BooleanProperty LEFT_TANK = BooleanProperty.create("left_tank");
	public static final BooleanProperty RIGHT_TANK = BooleanProperty.create("right_tank");
	public static final BooleanProperty BATTERY = BooleanProperty.create("battery");
	public static final BooleanProperty OPEN = BlockStateProperties.OPEN;

	public static final DirectionProperty FACING = BlockStateProperties.HORIZONTAL_FACING;
	private static final int BEDROCK_RESISTANCE = 3600000;

	public BackpackBlock() {
		this(0.8F);
	}

	public BackpackBlock(float explosionResistance) {
		super(Properties.of().mapColor(MapColor.WOOL).noOcclusion().strength(0.8F, explosionResistance).sound(SoundType.WOOL).pushReaction(PushReaction.DESTROY));
		registerDefaultState(stateDefinition.any().setValue(FACING, Direction.NORTH).setValue(WATERLOGGED, false).setValue(LEFT_TANK, false).setValue(RIGHT_TANK, false).setValue(BATTERY, false).setValue(OPEN, false));
	}

	@Override
	public boolean hasAnalogOutputSignal(BlockState state) {
		return true;
	}

	@Override
	public int getAnalogOutputSignal(BlockState blockState, Level level, BlockPos pos) {
		return WorldHelper.getBlockEntity(level, pos, BackpackBlockEntity.class).map(t -> InventoryHelper.getAnalogOutputSignal(t.getBackpackWrapper().getInventoryForInputOutput())).orElse(0);
	}

	@Override
	public FluidState getFluidState(BlockState state) {
		return Boolean.TRUE.equals(state.getValue(WATERLOGGED)) ? Fluids.WATER.getSource(false) : super.getFluidState(state);
	}

	@Override
	public BlockState updateShape(BlockState stateIn, Direction facing, BlockState facingState, LevelAccessor level, BlockPos currentPos, BlockPos facingPos) {
		if (Boolean.TRUE.equals(stateIn.getValue(WATERLOGGED))) {
			level.scheduleTick(currentPos, Fluids.WATER, Fluids.WATER.getTickDelay(level));
		}

		return super.updateShape(stateIn, facing, facingState, level, currentPos, facingPos);
	}

	@Override
	protected void createBlockStateDefinition(StateDefinition.Builder<Block, BlockState> builder) {
		builder.add(FACING, WATERLOGGED, LEFT_TANK, RIGHT_TANK, BATTERY, OPEN);
	}

	@Override
	public void tick(BlockState state, ServerLevel level, BlockPos pos, RandomSource random) {
		WorldHelper.getBlockEntity(level, pos, BackpackBlockEntity.class).ifPresent(BackpackBlockEntity::recheckOpen);
	}

	@Override
	public float getExplosionResistance(BlockState state, BlockGetter world, BlockPos pos, Explosion explosion) {
		if (hasEverlastingUpgrade(world, pos)) {
			return BEDROCK_RESISTANCE;
		}
		return super.getExplosionResistance(state, world, pos, explosion);
	}

	private boolean hasEverlastingUpgrade(BlockGetter world, BlockPos pos) {
		return WorldHelper.getBlockEntity(world, pos, BackpackBlockEntity.class).map(be -> !be.getBackpackWrapper().getUpgradeHandler().getTypeWrappers(EverlastingUpgradeItem.TYPE).isEmpty()).orElse(false);
	}

	@Override
	public VoxelShape getShape(BlockState state, BlockGetter worldIn, BlockPos pos, CollisionContext context) {
		return BackpackShapes.getShape(state);
	}

	@Override
	public VoxelShape getCollisionShape(BlockState state, BlockGetter level, BlockPos pos, CollisionContext context) {
		return BackpackShapes.getAuthoritativeShapeProvider().getShape(state);
	}

	@Override
	public VoxelShape getBlockSupportShape(BlockState state, BlockGetter level, BlockPos pos) {
		return BackpackShapes.getAuthoritativeShapeProvider().getShape(state);
	}

	@Override
	public VoxelShape getVisualShape(BlockState state, BlockGetter level, BlockPos pos, CollisionContext context) {
		return BackpackShapes.getAuthoritativeShapeProvider().getShape(state);
	}

	@Nullable
	@Override
	public BlockEntity newBlockEntity(BlockPos pos, BlockState state) {
		return new BackpackBlockEntity(pos, state);
	}

	@Override
	public InteractionResult useWithoutItem(BlockState state, Level level, BlockPos pos, Player player, BlockHitResult hit) {
		if (level.isClientSide) {
			return InteractionResult.SUCCESS;
		}

		ItemStack heldItem = player.getMainHandItem();
		if (player.isShiftKeyDown() && heldItem.isEmpty()) {
			if (hasPermissionsToPickup(player, pos)) {
				putInPlayersHandAndRemove(state, level, pos, player, InteractionHand.MAIN_HAND);
				return InteractionResult.SUCCESS;
			} else {
				return InteractionResult.FAIL;
			}

		}

		BackpackContext.Block backpackContext = new BackpackContext.Block(pos);

		player.openMenu(new SimpleMenuProvider((w, p, pl) -> new BackpackContainer(w, pl, backpackContext), getBackpackDisplayName(level, pos)), backpackContext::toBuffer);
		level.gameEvent(player, GameEvent.CONTAINER_OPEN, pos);
		return InteractionResult.SUCCESS;
	}

	private static boolean hasPermissionsToPickup(Player player, BlockPos pos) {
		return WorldHelper.getBlockEntity(player.level(), pos, BackpackBlockEntity.class).map(be -> {
			if (be.getStorageWrapper().getUpgradeHandler().getTypeWrappers(InfinityUpgradeItem.TYPE).stream().anyMatch(w -> !player.hasPermissions(w.getPermissionLevel()))) {
				player.displayClientMessage(SBPTranslationHelper.INSTANCE.translStatusMessage("infinity_upgrade_only_admin_pickup").withStyle(ChatFormatting.RED), true);
				return false;
			}
			return true;
		}).orElse(true);
	}

	@Override
	protected ItemInteractionResult useItemOn(ItemStack stack, BlockState state, Level level, BlockPos pos, Player player, InteractionHand hand, BlockHitResult hitResult) {
		if (!stack.isEmpty() && stack.getCapability(Capabilities.FluidHandler.ITEM) instanceof IFluidHandlerItem) {
			WorldHelper.getBlockEntity(level, pos, BackpackBlockEntity.class)
					.flatMap(be -> be.getBackpackWrapper().getFluidHandler()).ifPresent(backpackFluidHandler ->
							CapabilityHelper.runOnItemHandler(player, playerInventory -> {
								FluidActionResult resultOfEmptying = FluidUtil.tryEmptyContainerAndStow(stack, backpackFluidHandler, playerInventory, FluidType.BUCKET_VOLUME, player, true);
								if (resultOfEmptying.isSuccess()) {
									player.setItemInHand(InteractionHand.MAIN_HAND, resultOfEmptying.getResult());
								} else {
									FluidActionResult resultOfFilling = FluidUtil.tryFillContainerAndStow(stack, backpackFluidHandler, playerInventory, FluidType.BUCKET_VOLUME, player, true);
									if (resultOfFilling.isSuccess()) {
										player.setItemInHand(InteractionHand.MAIN_HAND, resultOfFilling.getResult());
									}
								}
							}));
			return ItemInteractionResult.SUCCESS;
		}
		return super.useItemOn(stack, state, level, pos, player, hand, hitResult);
	}

	private Component getBackpackDisplayName(Level level, BlockPos pos) {
		Component defaultDisplayName = new ItemStack(ModItems.BACKPACK.get()).getHoverName();
		return WorldHelper.getBlockEntity(level, pos, BackpackBlockEntity.class).map(be -> be.getBackpackWrapper().getBackpack().getHoverName()).orElse(defaultDisplayName);
	}

	private static void putInPlayersHandAndRemove(BlockState state, Level level, BlockPos pos, Player player, InteractionHand hand) {
		ItemStack backpack = WorldHelper.getBlockEntity(level, pos, BackpackBlockEntity.class).map(be -> be.getBackpackWrapper().getBackpack()).orElse(ItemStack.EMPTY);
		stopBackpackSounds(backpack, level, pos);

		player.setItemInHand(hand, backpack.copy());
		player.getCooldowns().addCooldown(backpack.getItem(), 5);
		level.removeBlock(pos, false);

		SoundType soundType = state.getSoundType();
		level.playSound(null, pos, soundType.getBreakSound(), SoundSource.BLOCKS, (soundType.getVolume() + 1.0F) / 2.0F, soundType.getPitch() * 0.8F);
	}

	@Override
	public void onRemove(BlockState state, Level level, BlockPos pos, BlockState newState, boolean isMoving) {
		if (!state.is(newState.getBlock())) {
			WorldHelper.getBlockEntity(level, pos, BackpackBlockEntity.class).ifPresent(IControllableStorage::removeFromController);
		}

		super.onRemove(state, level, pos, newState, isMoving);
	}

	@Override
	public BlockState playerWillDestroy(Level level, BlockPos pos, BlockState state, Player player) {
		BlockState result = super.playerWillDestroy(level, pos, state, player);
		WorldHelper.getBlockEntity(level, pos, BackpackBlockEntity.class).ifPresent(IControllableStorage::removeFromController);
		return result;
	}

	private static void stopBackpackSounds(ItemStack backpack, Level level, BlockPos pos) {
		BackpackWrapper.fromStack(backpack).getContentsUuid().ifPresent(uuid ->
				ServerStorageSoundHandler.stopPlayingDisc(level, Vec3.atCenterOf(pos), uuid));
	}

	public static void playerInteract(PlayerInteractEvent.RightClickBlock event) {
		Player player = event.getEntity();
		Level level = player.level();
		BlockPos pos = event.getPos();

		if (!player.isShiftKeyDown() || !hasEmptyMainHandAndSomethingInOffhand(player) || didntInteractWithBackpack(event)) {
			return;
		}

		if (level.isClientSide) {
			event.setCanceled(true);
			event.setCancellationResult(InteractionResult.SUCCESS);
			return;
		}

		BlockState state = level.getBlockState(pos);
		if (!(state.getBlock() instanceof BackpackBlock)) {
			return;
		}

		if (!hasPermissionsToPickup(player, pos)) {
			event.setCanceled(true);
			event.setCancellationResult(InteractionResult.FAIL);
			return;
		}

		putInPlayersHandAndRemove(state, level, pos, player, player.getMainHandItem().isEmpty() ? InteractionHand.MAIN_HAND : InteractionHand.OFF_HAND);

		event.setCanceled(true);
		event.setCancellationResult(InteractionResult.SUCCESS);
	}

	private static boolean didntInteractWithBackpack(PlayerInteractEvent.RightClickBlock event) {
		return !(event.getLevel().getBlockState(event.getPos()).getBlock() instanceof BackpackBlock);
	}

	private static boolean hasEmptyMainHandAndSomethingInOffhand(Player player) {
		return player.getMainHandItem().isEmpty() && !player.getOffhandItem().isEmpty();
	}

	@Override
	public void entityInside(BlockState state, Level level, BlockPos pos, Entity entity) {
		super.entityInside(state, level, pos, entity);
		if (!level.isClientSide && entity instanceof ItemEntity itemEntity) {
			WorldHelper.getBlockEntity(level, pos, BackpackBlockEntity.class).ifPresent(be -> tryToPickup(level, itemEntity, be.getBackpackWrapper()));
		}
	}

	@Override
	public boolean canEntityDestroy(BlockState state, BlockGetter world, BlockPos pos, Entity entity) {
		if (hasEverlastingUpgrade(world, pos)) {
			return false;
		}
		return super.canEntityDestroy(state, world, pos, entity);
	}

	private void tryToPickup(Level level, ItemEntity itemEntity, IStorageWrapper w) {
		ItemStack remainingStack = itemEntity.getItem().copy();
		remainingStack = InventoryHelper.runPickupOnPickupResponseUpgrades(level, w.getUpgradeHandler(), remainingStack, false);
		if (remainingStack.getCount() < itemEntity.getItem().getCount()) {
			itemEntity.setItem(remainingStack);
		}
	}

	@Nullable
	@Override
	public <T extends BlockEntity> BlockEntityTicker<T> getTicker(Level level, BlockState state, BlockEntityType<T> blockEntityType) {
		return !level.isClientSide ? createTickerHelper(blockEntityType, ModBlocks.BACKPACK_TILE_TYPE.get(), (l, blockPos, blockState, backpackBlockEntity) -> BackpackBlockEntity.serverTick(l, blockPos, backpackBlockEntity)) : null;
	}

	@Nullable
	protected static <E extends BlockEntity, A extends BlockEntity> BlockEntityTicker<A> createTickerHelper(BlockEntityType<A> typePassedIn, BlockEntityType<E> typeExpected, BlockEntityTicker<? super E> blockEntityTicker) {
		//noinspection unchecked
		return typeExpected == typePassedIn ? (BlockEntityTicker<A>) blockEntityTicker : null;
	}

	@Override
	public void animateTick(BlockState state, Level level, BlockPos pos, RandomSource rand) {
		WorldHelper.getBlockEntity(level, pos, BackpackBlockEntity.class).ifPresent(be -> {
			RenderInfo renderInfo = be.getBackpackWrapper().getRenderInfo();
			renderUpgrades(level, rand, pos, state.getValue(FACING), renderInfo);
		});

	}

	private static void renderUpgrades(Level level, RandomSource rand, BlockPos pos, Direction facing, RenderInfo renderInfo) {
		if (Minecraft.getInstance().isPaused()) {
			return;
		}
		renderInfo.getUpgradeRenderData().forEach((type, data) -> UpgradeRenderRegistry.getUpgradeRenderer(type).ifPresent(renderer -> renderUpgrade(renderer, level, rand, pos, facing, type, data)));
	}

	private static Vector3f getBackpackMiddleFacePoint(BlockPos pos, Direction facing, Vector3f vector) {
		Vector3f point = new Vector3f(vector);
		point.add(0, 0, 0.41f);
		point.rotate(Axis.YN.rotationDegrees(facing.toYRot()));
		point.add(pos.getX() + 0.5f, pos.getY(), pos.getZ() + 0.5f);
		return point;
	}

	private static <T extends IUpgradeRenderData> void renderUpgrade(IUpgradeRenderer<T> renderer, Level level, RandomSource rand, BlockPos pos, Direction facing, UpgradeRenderDataType<?> type, IUpgradeRenderData data) {
		//noinspection unchecked
		type.cast(data).ifPresent(renderData -> renderer.render(level, rand, vector -> getBackpackMiddleFacePoint(pos, facing, vector), (T) renderData));
	}
}
