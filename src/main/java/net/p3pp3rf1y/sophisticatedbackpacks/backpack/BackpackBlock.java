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
import net.minecraft.world.SimpleMenuProvider;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.InsideBlockEffectApplier;
import net.minecraft.world.entity.item.ItemEntity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.*;
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
import net.minecraft.world.level.block.state.properties.EnumProperty;
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
import net.neoforged.neoforge.transfer.ResourceHandler;
import net.neoforged.neoforge.transfer.ResourceHandlerUtil;
import net.neoforged.neoforge.transfer.access.ItemAccess;
import net.neoforged.neoforge.transfer.fluid.FluidResource;
import net.neoforged.neoforge.transfer.fluid.FluidUtil;
import net.neoforged.neoforge.transfer.item.ItemResource;
import net.neoforged.neoforge.transfer.resource.ResourceStack;
import net.neoforged.neoforge.transfer.transaction.Transaction;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackTranslationHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContext;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModBlocks;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.everlasting.EverlastingUpgradeItem;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.api.IUpgradeClientTickHandler;
import net.p3pp3rf1y.sophisticatedcore.client.render.UpgradeClientRegistry;
import net.p3pp3rf1y.sophisticatedcore.controller.IControllableStorage;
import net.p3pp3rf1y.sophisticatedcore.renderdata.IUpgradeClientData;
import net.p3pp3rf1y.sophisticatedcore.renderdata.RenderDataHandler;
import net.p3pp3rf1y.sophisticatedcore.renderdata.UpgradeClientDataType;
import net.p3pp3rf1y.sophisticatedcore.upgrades.infinity.InfinityUpgradeItem;
import net.p3pp3rf1y.sophisticatedcore.upgrades.jukebox.ServerStorageSoundHandler;
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

	public static final EnumProperty<Direction> FACING = BlockStateProperties.HORIZONTAL_FACING;
	private static final int BEDROCK_RESISTANCE = 3600000;

	public BackpackBlock(Properties properties) {
		this(0.8F, properties);
	}

	public BackpackBlock(float explosionResistance, Properties properties) {
		super(properties.mapColor(MapColor.WOOL).noOcclusion().strength(0.8F, explosionResistance).sound(SoundType.WOOL).pushReaction(PushReaction.DESTROY));
		registerDefaultState(stateDefinition.any().setValue(FACING, Direction.NORTH).setValue(WATERLOGGED, false).setValue(LEFT_TANK, false)
				.setValue(RIGHT_TANK, false).setValue(BATTERY, false).setValue(OPEN, false));
	}

	@Override
	public boolean hasAnalogOutputSignal(BlockState state) {
		return true;
	}

	@Override
	public int getAnalogOutputSignal(BlockState blockState, Level level, BlockPos pos, Direction direction) {
		return WorldHelper.getBlockEntity(level, pos, BackpackBlockEntity.class)
				.map(t -> InventoryHelper.getAnalogOutputSignal(t.getBackpackWrapper().getInventoryHandler())).orElse(0);
	}

	@Override
	public FluidState getFluidState(BlockState state) {
		return Boolean.TRUE.equals(state.getValue(WATERLOGGED)) ? Fluids.WATER.getSource(false) : super.getFluidState(state);
	}

	@Override
	protected BlockState updateShape(BlockState state, LevelReader level, ScheduledTickAccess scheduledTickAccess, BlockPos pos, Direction direction,
			BlockPos neighborPos, BlockState neighborState, RandomSource random) {
		if (Boolean.TRUE.equals(state.getValue(WATERLOGGED))) {
			scheduledTickAccess.scheduleTick(pos, Fluids.WATER, Fluids.WATER.getTickDelay(level));
		}

		return super.updateShape(state, level, scheduledTickAccess, pos, direction, neighborPos, neighborState, random);
	}

	@Override
	protected void createBlockStateDefinition(StateDefinition.Builder<Block, BlockState> builder) {
		builder.add(FACING, WATERLOGGED, LEFT_TANK, RIGHT_TANK, BATTERY, OPEN);
	}

	@Override
	protected void tick(BlockState state, ServerLevel level, BlockPos pos, RandomSource random) {
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
		return WorldHelper.getBlockEntity(world, pos, BackpackBlockEntity.class)
				.map(be -> !be.getBackpackWrapper().getUpgradeHandler().getTypeWrappers(EverlastingUpgradeItem.TYPE).isEmpty()).orElse(false);
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
		if (level.isClientSide()) {
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

		player.openMenu(new SimpleMenuProvider((w, p, pl) -> new BackpackContainer(w, pl, backpackContext), getBackpackDisplayName(level, pos)),
				backpackContext::toBuffer);
		level.gameEvent(player, GameEvent.CONTAINER_OPEN, pos);
		return InteractionResult.SUCCESS;
	}

	private static boolean hasPermissionsToPickup(Player player, BlockPos pos) {
		return WorldHelper.getBlockEntity(player.level(), pos, BackpackBlockEntity.class).map(be -> {
			if (be.getStorageWrapper().getUpgradeHandler().getTypeWrappers(InfinityUpgradeItem.TYPE).stream()
					.anyMatch(w -> !player.hasPermissions(w.getPermissionLevel()))) {
				player.displayClientMessage(
						BackpackTranslationHelper.INSTANCE.translStatusMessage("infinity_upgrade_only_admin_pickup").withStyle(ChatFormatting.RED), true);
				return false;
			}
			return true;
		}).orElse(true);
	}

	@Override
	protected InteractionResult useItemOn(ItemStack stack, BlockState state, Level level, BlockPos pos, Player player, InteractionHand hand,
			BlockHitResult hitResult) {
		if (!stack.isEmpty() && stack.getCapability(Capabilities.Fluid.ITEM, ItemAccess.forStack(stack)) != null) {
			if (player.hasInfiniteMaterials() && interactWithBackpackFluidHandlerInCreative(level, pos, hitResult.getDirection(), player, stack)) {
				return InteractionResult.SUCCESS.heldItemTransformedTo(player.getItemInHand(hand));
			}
			if (FluidUtil.interactWithFluidHandler(player, hand, level, pos, hitResult.getDirection())) {
				return InteractionResult.SUCCESS.heldItemTransformedTo(player.getItemInHand(hand));
			}
		}
		return super.useItemOn(stack, state, level, pos, player, hand, hitResult);
	}

	private boolean interactWithBackpackFluidHandlerInCreative(Level level, BlockPos pos, Direction side, Player player, ItemStack stack) {
		ResourceHandler<FluidResource> blockHandler = level.getCapability(Capabilities.Fluid.BLOCK, pos, side);
		ResourceHandler<FluidResource> itemHandler = BackpackWrapper.fromStack(stack).getItemFluidHandler().orElse(null);
		if (blockHandler == null || itemHandler == null) {
			return false;
		}

		ResourceStack<FluidResource> moved = ResourceHandlerUtil.moveFirst(blockHandler, itemHandler, fluidResource -> true, Integer.MAX_VALUE, null);
		boolean pickup = true;
		if (moved == null) {
			moved = ResourceHandlerUtil.moveFirst(itemHandler, blockHandler, fluidResource -> true, Integer.MAX_VALUE, null);
			pickup = false;
		}

		if (moved != null) {
			return true;
		}

		return false;
	}

	private Component getBackpackDisplayName(Level level, BlockPos pos) {
		Component defaultDisplayName = new ItemStack(ModItems.BACKPACK.get()).getHoverName();
		return WorldHelper.getBlockEntity(level, pos, BackpackBlockEntity.class).map(be -> be.getBackpackWrapper().getBackpack().getHoverName())
				.orElse(defaultDisplayName);
	}

	private static void putInPlayersHandAndRemove(BlockState state, Level level, BlockPos pos, Player player, InteractionHand hand) {
		ItemStack backpack = WorldHelper.getBlockEntity(level, pos, BackpackBlockEntity.class).map(be -> be.getBackpackWrapper().getBackpack())
				.orElse(ItemStack.EMPTY);
		stopBackpackSounds(backpack, level, pos);

		player.setItemInHand(hand, backpack.copy());
		player.getCooldowns().addCooldown(backpack, 5);
		level.removeBlock(pos, false);

		SoundType soundType = state.getSoundType();
		level.playSound(null, pos, soundType.getBreakSound(), SoundSource.BLOCKS, (soundType.getVolume() + 1.0F) / 2.0F, soundType.getPitch() * 0.8F);
	}

	@Override
	public BlockState playerWillDestroy(Level level, BlockPos pos, BlockState state, Player player) {
		BlockState result = super.playerWillDestroy(level, pos, state, player);
		WorldHelper.getBlockEntity(level, pos, BackpackBlockEntity.class).ifPresent(IControllableStorage::removeFromController);
		return result;
	}

	private static void stopBackpackSounds(ItemStack backpack, Level level, BlockPos pos) {
		BackpackWrapper.fromStack(backpack).getContentsUuid().ifPresent(uuid -> ServerStorageSoundHandler.stopPlayingDisc(level, Vec3.atCenterOf(pos), uuid));
	}

	public static void playerInteract(PlayerInteractEvent.RightClickBlock event) {
		Player player = event.getEntity();
		Level level = player.level();
		BlockPos pos = event.getPos();

		if (!player.isShiftKeyDown() || !hasEmptyMainHandAndSomethingInOffhand(player) || didntInteractWithBackpack(event)) {
			return;
		}

		if (level.isClientSide()) {
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
	protected void entityInside(BlockState state, Level level, BlockPos pos, Entity entity, InsideBlockEffectApplier effectApplier, boolean flag) {
		super.entityInside(state, level, pos, entity, effectApplier, flag);
		if (!level.isClientSide() && entity instanceof ItemEntity itemEntity) {
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
		try (Transaction tx = Transaction.openRoot()) {
			ItemStack stack = itemEntity.getItem();
			int pickecUp = InventoryHelper.runPickupOnPickupResponseUpgrades(level, w.getUpgradeHandler(), ItemResource.of(stack), stack.getCount(), tx);
			if (pickecUp > 0) {
				tx.commit();
				int remaining = stack.getCount() - pickecUp;
				itemEntity.setItem(remaining == 0 ? ItemStack.EMPTY : stack.copyWithCount(remaining));
			}
		}
	}

	@Nullable
	@Override
	public <T extends BlockEntity> BlockEntityTicker<T> getTicker(Level level, BlockState state, BlockEntityType<T> blockEntityType) {
		return !level.isClientSide()
				? createTickerHelper(blockEntityType, ModBlocks.BACKPACK_TILE_TYPE.get(),
						(l, blockPos, blockState, backpackBlockEntity) -> BackpackBlockEntity.serverTick(l, blockPos, backpackBlockEntity))
				: null;
	}

	@Nullable
	protected static <E extends BlockEntity, A extends BlockEntity> BlockEntityTicker<A> createTickerHelper(BlockEntityType<A> typePassedIn,
			BlockEntityType<E> typeExpected, BlockEntityTicker<? super E> blockEntityTicker) {
		// noinspection unchecked
		return typeExpected == typePassedIn ? (BlockEntityTicker<A>) blockEntityTicker : null;
	}

	@Override
	public void animateTick(BlockState state, Level level, BlockPos pos, RandomSource rand) {
		WorldHelper.getBlockEntity(level, pos, BackpackBlockEntity.class).ifPresent(be -> {
			RenderDataHandler renderDataHandler = be.getBackpackWrapper().getRenderDataHandler();
			renderUpgrades(level, rand, pos, state.getValue(FACING), renderDataHandler);
		});

	}

	private static void renderUpgrades(Level level, RandomSource rand, BlockPos pos, Direction facing, RenderDataHandler renderDataHandler) {
		if (Minecraft.getInstance().isPaused()) {
			return;
		}
		renderDataHandler.getUpgradeClientData().forEach((type, data) -> UpgradeClientRegistry.getUpgradeClientTickHandler(type)
				.ifPresent(renderer -> clientTickUpgrade(renderer, level, rand, pos, facing, type, data)));
	}

	private static Vector3f getBackpackMiddleFacePoint(BlockPos pos, Direction facing, Vector3f vector) {
		Vector3f point = new Vector3f(vector);
		point.add(0, 0, 0.41f);
		point.rotate(Axis.YN.rotationDegrees(facing.toYRot()));
		point.add(pos.getX() + 0.5f, pos.getY(), pos.getZ() + 0.5f);
		return point;
	}

	private static <T extends IUpgradeClientData> void clientTickUpgrade(IUpgradeClientTickHandler<T> renderer, Level level, RandomSource rand, BlockPos pos,
			Direction facing, UpgradeClientDataType<?> type, IUpgradeClientData data) {
		// noinspection unchecked
		type.cast(data).ifPresent(clientData -> renderer.onClientTick(level, rand, vector -> getBackpackMiddleFacePoint(pos, facing, vector), (T) clientData));
	}
}
