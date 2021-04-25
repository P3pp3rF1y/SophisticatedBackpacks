package net.p3pp3rf1y.sophisticatedbackpacks.backpack;

import net.minecraft.block.Block;
import net.minecraft.block.BlockState;
import net.minecraft.block.IWaterLoggable;
import net.minecraft.block.SoundType;
import net.minecraft.block.material.Material;
import net.minecraft.entity.Entity;
import net.minecraft.entity.item.ItemEntity;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraft.fluid.FluidState;
import net.minecraft.fluid.Fluids;
import net.minecraft.inventory.container.SimpleNamedContainerProvider;
import net.minecraft.item.ItemStack;
import net.minecraft.state.DirectionProperty;
import net.minecraft.state.StateContainer;
import net.minecraft.state.properties.BlockStateProperties;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.ActionResultType;
import net.minecraft.util.Direction;
import net.minecraft.util.Hand;
import net.minecraft.util.SoundCategory;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.BlockRayTraceResult;
import net.minecraft.util.math.shapes.IBooleanFunction;
import net.minecraft.util.math.shapes.ISelectionContext;
import net.minecraft.util.math.shapes.VoxelShape;
import net.minecraft.util.math.shapes.VoxelShapes;
import net.minecraft.util.math.vector.Vector3d;
import net.minecraft.util.text.ITextComponent;
import net.minecraft.world.Explosion;
import net.minecraft.world.IBlockReader;
import net.minecraft.world.IWorld;
import net.minecraft.world.World;
import net.minecraft.world.server.ServerWorld;
import net.minecraftforge.event.entity.player.PlayerInteractEvent;
import net.minecraftforge.fml.network.NetworkHooks;
import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContext;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.everlasting.EverlastingUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.jukebox.ServerBackpackSoundHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.util.InventoryHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.WorldHelper;

import javax.annotation.Nullable;

import java.util.stream.Stream;

import static net.minecraft.state.properties.BlockStateProperties.WATERLOGGED;

public class BackpackBlock extends Block implements IWaterLoggable {
	public static final DirectionProperty FACING = BlockStateProperties.HORIZONTAL_FACING;
	private static final VoxelShape NORTH_SHAPE = Stream.of(
			Block.makeCuboidShape(5, 3, 2.75, 6, 5, 3),
			Block.makeCuboidShape(10, 3, 2.75, 11, 5, 3),
			Block.makeCuboidShape(4, 0, 3, 12, 1, 5),
			Block.makeCuboidShape(4, 1, 3, 12, 2, 5),
			Block.makeCuboidShape(4, 2, 3, 12, 6, 5),
			Block.makeCuboidShape(15, 3, 7.5, 15.25, 5, 8.5),
			Block.makeCuboidShape(13, 0, 5.5, 15, 1, 10.5),
			Block.makeCuboidShape(13, 1, 5.5, 15, 2, 10.5),
			Block.makeCuboidShape(13, 2, 5.5, 15, 6, 10.5),
			Block.makeCuboidShape(14, 8, 7.5, 14.25, 10, 8.5),
			Block.makeCuboidShape(13, 7, 5.5, 14, 11, 10.5),
			Block.makeCuboidShape(1.75, 8, 7.5, 2, 10, 8.5),
			Block.makeCuboidShape(2, 7, 5.5, 3, 11, 10.5),
			Block.makeCuboidShape(0.75, 3, 7.5, 1, 5, 8.5),
			Block.makeCuboidShape(1, 0, 5.5, 3, 1, 10.5),
			Block.makeCuboidShape(1, 1, 5.5, 3, 2, 10.5),
			Block.makeCuboidShape(1, 2, 5.5, 3, 6, 10.5),
			Block.makeCuboidShape(11.75, 0, 11, 12.25, 12.5, 12),
			Block.makeCuboidShape(10.75, 0, 11, 11.75, 12.5, 12),
			Block.makeCuboidShape(10.25, 0, 11, 10.75, 12.5, 12),
			Block.makeCuboidShape(3.75, 0, 11, 4.25, 12.5, 12),
			Block.makeCuboidShape(4.25, 0, 11, 5.25, 12.5, 12),
			Block.makeCuboidShape(5.25, 0, 11, 5.75, 12.5, 12),
			Block.makeCuboidShape(11.75, 8.25, 4.75, 12.75, 13.25, 10.75),
			Block.makeCuboidShape(4.25, 9.25, 4.75, 11.75, 13.25, 10.75),
			Block.makeCuboidShape(3.25, 8.25, 4.75, 4.25, 13.25, 10.75),
			Block.makeCuboidShape(4.25, 8.25, 4.75, 11.75, 9.25, 5),
			Block.makeCuboidShape(5.75, 13.25, 7.75, 6.5, 14, 8.75),
			Block.makeCuboidShape(5.75, 14, 7.75, 10.25, 14.5, 8.75),
			Block.makeCuboidShape(9.5, 13.25, 7.75, 10.25, 14, 8.75),
			Block.makeCuboidShape(4.5, 7, 4.5, 5.5, 9, 5.25),
			Block.makeCuboidShape(4.5, 9, 4.5, 5.5, 13.5, 11.25),
			Block.makeCuboidShape(10.5, 7, 4.5, 11.5, 9, 5.25),
			Block.makeCuboidShape(10.5, 9, 4.5, 11.5, 13.5, 11.25),
			Block.makeCuboidShape(4, 1, 11, 12, 12, 11.5),
			Block.makeCuboidShape(3, 0, 5, 13, 13, 11)
	).reduce((v1, v2) -> {return VoxelShapes.combineAndSimplify(v1, v2, IBooleanFunction.OR);}).get();
	private static final VoxelShape SOUTH_SHAPE = Stream.of(
			Block.makeCuboidShape(10, 3, 13, 11, 5, 13.25),
			Block.makeCuboidShape(5, 3, 13, 6, 5, 13.25),
			Block.makeCuboidShape(4, 0, 11, 12, 1, 13),
			Block.makeCuboidShape(4, 1, 11, 12, 2, 13),
			Block.makeCuboidShape(4, 2, 11, 12, 6, 13),
			Block.makeCuboidShape(0.75, 3, 7.5, 1, 5, 8.5),
			Block.makeCuboidShape(1, 0, 5.5, 3, 1, 10.5),
			Block.makeCuboidShape(1, 1, 5.5, 3, 2, 10.5),
			Block.makeCuboidShape(1, 2, 5.5, 3, 6, 10.5),
			Block.makeCuboidShape(1.75, 8, 7.5, 2, 10, 8.5),
			Block.makeCuboidShape(2, 7, 5.5, 3, 11, 10.5),
			Block.makeCuboidShape(14, 8, 7.5, 14.25, 10, 8.5),
			Block.makeCuboidShape(13, 7, 5.5, 14, 11, 10.5),
			Block.makeCuboidShape(15, 3, 7.5, 15.25, 5, 8.5),
			Block.makeCuboidShape(13, 0, 5.5, 15, 1, 10.5),
			Block.makeCuboidShape(13, 1, 5.5, 15, 2, 10.5),
			Block.makeCuboidShape(13, 2, 5.5, 15, 6, 10.5),
			Block.makeCuboidShape(3.75, 0, 4, 4.25, 12.5, 5),
			Block.makeCuboidShape(4.25, 0, 4, 5.25, 12.5, 5),
			Block.makeCuboidShape(5.25, 0, 4, 5.75, 12.5, 5),
			Block.makeCuboidShape(11.75, 0, 4, 12.25, 12.5, 5),
			Block.makeCuboidShape(10.75, 0, 4, 11.75, 12.5, 5),
			Block.makeCuboidShape(10.25, 0, 4, 10.75, 12.5, 5),
			Block.makeCuboidShape(3.25, 8.25, 5.25, 4.25, 13.25, 11.25),
			Block.makeCuboidShape(4.25, 9.25, 5.25, 11.75, 13.25, 11.25),
			Block.makeCuboidShape(11.75, 8.25, 5.25, 12.75, 13.25, 11.25),
			Block.makeCuboidShape(4.25, 8.25, 11, 11.75, 9.25, 11.25),
			Block.makeCuboidShape(9.5, 13.25, 7.25, 10.25, 14, 8.25),
			Block.makeCuboidShape(5.75, 14, 7.25, 10.25, 14.5, 8.25),
			Block.makeCuboidShape(5.75, 13.25, 7.25, 6.5, 14, 8.25),
			Block.makeCuboidShape(10.5, 7, 10.75, 11.5, 9, 11.5),
			Block.makeCuboidShape(10.5, 9, 4.75, 11.5, 13.5, 11.5),
			Block.makeCuboidShape(4.5, 7, 10.75, 5.5, 9, 11.5),
			Block.makeCuboidShape(4.5, 9, 4.75, 5.5, 13.5, 11.5),
			Block.makeCuboidShape(4, 1, 4.5, 12, 12, 5),
			Block.makeCuboidShape(3, 0, 5, 13, 13, 11)
	).reduce((v1, v2) -> {return VoxelShapes.combineAndSimplify(v1, v2, IBooleanFunction.OR);}).get();
	private static final VoxelShape WEST_SHAPE = Stream.of(
			Block.makeCuboidShape(2.75, 3, 10, 3, 5, 11),
			Block.makeCuboidShape(2.75, 3, 5, 3, 5, 6),
			Block.makeCuboidShape(3, 0, 4, 5, 1, 12),
			Block.makeCuboidShape(3, 1, 4, 5, 2, 12),
			Block.makeCuboidShape(3, 2, 4, 5, 6, 12),
			Block.makeCuboidShape(7.5, 3, 0.75, 8.5, 5, 1),
			Block.makeCuboidShape(5.5, 0, 1, 10.5, 1, 3),
			Block.makeCuboidShape(5.5, 1, 1, 10.5, 2, 3),
			Block.makeCuboidShape(5.5, 2, 1, 10.5, 6, 3),
			Block.makeCuboidShape(7.5, 8, 1.75, 8.5, 10, 2),
			Block.makeCuboidShape(5.5, 7, 2, 10.5, 11, 3),
			Block.makeCuboidShape(7.5, 8, 14, 8.5, 10, 14.25),
			Block.makeCuboidShape(5.5, 7, 13, 10.5, 11, 14),
			Block.makeCuboidShape(7.5, 3, 15, 8.5, 5, 15.25),
			Block.makeCuboidShape(5.5, 0, 13, 10.5, 1, 15),
			Block.makeCuboidShape(5.5, 1, 13, 10.5, 2, 15),
			Block.makeCuboidShape(5.5, 2, 13, 10.5, 6, 15),
			Block.makeCuboidShape(11, 0, 3.75, 12, 12.5, 4.25),
			Block.makeCuboidShape(11, 0, 4.25, 12, 12.5, 5.25),
			Block.makeCuboidShape(11, 0, 5.25, 12, 12.5, 5.75),
			Block.makeCuboidShape(11, 0, 11.75, 12, 12.5, 12.25),
			Block.makeCuboidShape(11, 0, 10.75, 12, 12.5, 11.75),
			Block.makeCuboidShape(11, 0, 10.25, 12, 12.5, 10.75),
			Block.makeCuboidShape(4.75, 8.25, 3.25, 10.75, 13.25, 4.25),
			Block.makeCuboidShape(4.75, 9.25, 4.25, 10.75, 13.25, 11.75),
			Block.makeCuboidShape(4.75, 8.25, 11.75, 10.75, 13.25, 12.75),
			Block.makeCuboidShape(4.75, 8.25, 4.25, 5, 9.25, 11.75),
			Block.makeCuboidShape(7.75, 13.25, 9.5, 8.75, 14, 10.25),
			Block.makeCuboidShape(7.75, 14, 5.75, 8.75, 14.5, 10.25),
			Block.makeCuboidShape(7.75, 13.25, 5.75, 8.75, 14, 6.5),
			Block.makeCuboidShape(4.5, 7, 10.5, 5.25, 9, 11.5),
			Block.makeCuboidShape(4.5, 9, 10.5, 11.25, 13.5, 11.5),
			Block.makeCuboidShape(4.5, 7, 4.5, 5.25, 9, 5.5),
			Block.makeCuboidShape(4.5, 9, 4.5, 11.25, 13.5, 5.5),
			Block.makeCuboidShape(11, 1, 4, 11.5, 12, 12),
			Block.makeCuboidShape(5, 0, 3, 11, 13, 13)
	).reduce((v1, v2) -> {return VoxelShapes.combineAndSimplify(v1, v2, IBooleanFunction.OR);}).get();
	private static final VoxelShape EAST_SHAPE = Stream.of(
			Block.makeCuboidShape(13, 3, 5, 13.25, 5, 6),
			Block.makeCuboidShape(13, 3, 10, 13.25, 5, 11),
			Block.makeCuboidShape(11, 0, 4, 13, 1, 12),
			Block.makeCuboidShape(11, 1, 4, 13, 2, 12),
			Block.makeCuboidShape(11, 2, 4, 13, 6, 12),
			Block.makeCuboidShape(7.5, 3, 15, 8.5, 5, 15.25),
			Block.makeCuboidShape(5.5, 0, 13, 10.5, 1, 15),
			Block.makeCuboidShape(5.5, 1, 13, 10.5, 2, 15),
			Block.makeCuboidShape(5.5, 2, 13, 10.5, 6, 15),
			Block.makeCuboidShape(7.5, 8, 14, 8.5, 10, 14.25),
			Block.makeCuboidShape(5.5, 7, 13, 10.5, 11, 14),
			Block.makeCuboidShape(7.5, 8, 1.75, 8.5, 10, 2),
			Block.makeCuboidShape(5.5, 7, 2, 10.5, 11, 3),
			Block.makeCuboidShape(7.5, 3, 0.75, 8.5, 5, 1),
			Block.makeCuboidShape(5.5, 0, 1, 10.5, 1, 3),
			Block.makeCuboidShape(5.5, 1, 1, 10.5, 2, 3),
			Block.makeCuboidShape(5.5, 2, 1, 10.5, 6, 3),
			Block.makeCuboidShape(4, 0, 11.75, 5, 12.5, 12.25),
			Block.makeCuboidShape(4, 0, 10.75, 5, 12.5, 11.75),
			Block.makeCuboidShape(4, 0, 10.25, 5, 12.5, 10.75),
			Block.makeCuboidShape(4, 0, 3.75, 5, 12.5, 4.25),
			Block.makeCuboidShape(4, 0, 4.25, 5, 12.5, 5.25),
			Block.makeCuboidShape(4, 0, 5.25, 5, 12.5, 5.75),
			Block.makeCuboidShape(5.25, 8.25, 11.75, 11.25, 13.25, 12.75),
			Block.makeCuboidShape(5.25, 9.25, 4.25, 11.25, 13.25, 11.75),
			Block.makeCuboidShape(5.25, 8.25, 3.25, 11.25, 13.25, 4.25),
			Block.makeCuboidShape(11, 8.25, 4.25, 11.25, 9.25, 11.75),
			Block.makeCuboidShape(7.25, 13.25, 5.75, 8.25, 14, 6.5),
			Block.makeCuboidShape(7.25, 14, 5.75, 8.25, 14.5, 10.25),
			Block.makeCuboidShape(7.25, 13.25, 9.5, 8.25, 14, 10.25),
			Block.makeCuboidShape(10.75, 7, 4.5, 11.5, 9, 5.5),
			Block.makeCuboidShape(4.75, 9, 4.5, 11.5, 13.5, 5.5),
			Block.makeCuboidShape(10.75, 7, 10.5, 11.5, 9, 11.5),
			Block.makeCuboidShape(4.75, 9, 10.5, 11.5, 13.5, 11.5),
			Block.makeCuboidShape(4.5, 1, 4, 5, 12, 12),
			Block.makeCuboidShape(5, 0, 3, 11, 13, 13)
	).reduce((v1, v2) -> {return VoxelShapes.combineAndSimplify(v1, v2, IBooleanFunction.OR);}).get();
	private static final int BEDROCK_RESISTANCE = 3600000;

	public BackpackBlock() {
		super(Properties.create(Material.WOOL).notSolid().hardnessAndResistance(0.8F).sound(SoundType.CLOTH));
		setDefaultState(stateContainer.getBaseState().with(FACING, Direction.NORTH).with(WATERLOGGED, false));
	}

	@Override
	public FluidState getFluidState(BlockState state) {
		return Boolean.TRUE.equals(state.get(WATERLOGGED)) ? Fluids.WATER.getStillFluidState(false) : super.getFluidState(state);
	}

	@Override
	public BlockState updatePostPlacement(BlockState stateIn, Direction facing, BlockState facingState, IWorld worldIn, BlockPos currentPos, BlockPos facingPos) {
		if (Boolean.TRUE.equals(stateIn.get(WATERLOGGED))) {
			worldIn.getPendingFluidTicks().scheduleTick(currentPos, Fluids.WATER, Fluids.WATER.getTickRate(worldIn));
		}

		return super.updatePostPlacement(stateIn, facing, facingState, worldIn, currentPos, facingPos);
	}

	@Override
	protected void fillStateContainer(StateContainer.Builder<Block, BlockState> builder) {
		builder.add(FACING, WATERLOGGED);
	}

	@Override
	public float getExplosionResistance(BlockState state, IBlockReader world, BlockPos pos, Explosion explosion) {
		if (hasEverlastingUpgrade(world, pos)) {
			return BEDROCK_RESISTANCE;
		}
		return super.getExplosionResistance(state, world, pos, explosion);
	}

	private boolean hasEverlastingUpgrade(IBlockReader world, BlockPos pos) {
		return WorldHelper.getTile(world, pos, BackpackTileEntity.class).map(te -> !te.getBackpackWrapper().getUpgradeHandler().getTypeWrappers(EverlastingUpgradeItem.TYPE).isEmpty()).orElse(false);
	}

	@Override
	public VoxelShape getShape(BlockState state, IBlockReader worldIn, BlockPos pos, ISelectionContext context) {
		Direction facing = state.get(FACING);

		switch (facing) {
			case NORTH:
				return NORTH_SHAPE;
			case SOUTH:
				return SOUTH_SHAPE;
			case WEST:
				return WEST_SHAPE;
			case EAST:
			default:
				return EAST_SHAPE;
		}
	}

	@Override
	public boolean hasTileEntity(BlockState state) {
		return true;
	}

	@Nullable
	@Override
	public TileEntity createTileEntity(BlockState state, IBlockReader world) {
		return new BackpackTileEntity();
	}

	@Override
	public ActionResultType onBlockActivated(BlockState state, World world, BlockPos pos, PlayerEntity player, Hand hand, BlockRayTraceResult hit) {
		if (world.isRemote) {
			return ActionResultType.SUCCESS;
		}

		if (player.isSneaking() && player.getHeldItem(hand).isEmpty()) {
			putInPlayersHandAndRemove(state, world, pos, player, hand);
			return ActionResultType.SUCCESS;
		}

		NetworkHooks.openGui((ServerPlayerEntity) player, new SimpleNamedContainerProvider((w, p, pl) -> new BackpackContainer(w, pl, new BackpackContext.Block(pos)),
				getBackpackDisplayName(world, pos)), buf -> buf.writeLong(pos.toLong()));
		return ActionResultType.SUCCESS;
	}

	private ITextComponent getBackpackDisplayName(World world, BlockPos pos) {
		ITextComponent defaultDisplayName = new ItemStack(ModItems.BACKPACK.get()).getDisplayName();
		return WorldHelper.getTile(world, pos, BackpackTileEntity.class).map(te -> te.getBackpackWrapper().getBackpack().getDisplayName()).orElse(defaultDisplayName);
	}

	private static void putInPlayersHandAndRemove(BlockState state, World world, BlockPos pos, PlayerEntity player, Hand hand) {
		ItemStack backpack = WorldHelper.getTile(world, pos, BackpackTileEntity.class).map(te -> te.getBackpackWrapper().getBackpack()).orElse(ItemStack.EMPTY);
		player.setHeldItem(hand, backpack);
		player.getCooldownTracker().setCooldown(backpack.getItem(), 5);
		world.removeBlock(pos, false);

		stopBackpackSounds(backpack, world, pos);

		SoundType soundType = state.getSoundType();
		world.playSound(null, pos, soundType.getBreakSound(), SoundCategory.BLOCKS, (soundType.getVolume() + 1.0F) / 2.0F, soundType.getPitch() * 0.8F);
	}

	private static void stopBackpackSounds(ItemStack backpack, World world, BlockPos pos) {
		backpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance()).ifPresent(wrapper -> wrapper.getContentsUuid().ifPresent(uuid ->
				ServerBackpackSoundHandler.stopPlayingDisc((ServerWorld) world, Vector3d.copyCentered(pos), uuid))
		);
	}

	public static void playerInteract(PlayerInteractEvent.RightClickBlock event) {
		PlayerEntity player = event.getPlayer();
		World world = player.world;
		BlockPos pos = event.getPos();

		if (!player.isSneaking() || !hasEmptyMainHandAndSomethingInOffhand(player) || didntInteractWithBackpack(event)) {
			return;
		}

		if (world.isRemote) {
			event.setCanceled(true);
			event.setCancellationResult(ActionResultType.SUCCESS);
			return;
		}

		BlockState state = world.getBlockState(pos);
		if (!(state.getBlock() instanceof BackpackBlock)) {
			return;
		}

		putInPlayersHandAndRemove(state, world, pos, player, player.getHeldItemMainhand().isEmpty() ? Hand.MAIN_HAND : Hand.OFF_HAND);

		event.setCanceled(true);
		event.setCancellationResult(ActionResultType.SUCCESS);
	}

	private static boolean didntInteractWithBackpack(PlayerInteractEvent.RightClickBlock event) {
		return !(event.getWorld().getBlockState(event.getPos()).getBlock() instanceof BackpackBlock);
	}

	private static boolean hasEmptyMainHandAndSomethingInOffhand(PlayerEntity player) {
		return player.getHeldItemMainhand().isEmpty() && !player.getHeldItemOffhand().isEmpty();
	}

	@Override
	public void onEntityCollision(BlockState state, World world, BlockPos pos, Entity entity) {
		super.onEntityCollision(state, world, pos, entity);
		if (entity instanceof ItemEntity) {
			ItemEntity itemEntity = (ItemEntity) entity;
			WorldHelper.getTile(world, pos, BackpackTileEntity.class).ifPresent(te -> tryToPickup(world, itemEntity, te.getBackpackWrapper()));
		}
	}

	@Override
	public boolean canEntityDestroy(BlockState state, IBlockReader world, BlockPos pos, Entity entity) {
		if (hasEverlastingUpgrade(world, pos)) {
			return false;
		}
		return super.canEntityDestroy(state, world, pos, entity);
	}

	private void tryToPickup(World world, ItemEntity itemEntity, IBackpackWrapper w) {
		ItemStack remainingStack = itemEntity.getItem().copy();
		InventoryHelper.runPickupOnBackpack(world, remainingStack, w, false);
		if (remainingStack.getCount() < itemEntity.getItem().getCount()) {
			itemEntity.setItem(remainingStack);
		}
	}
}