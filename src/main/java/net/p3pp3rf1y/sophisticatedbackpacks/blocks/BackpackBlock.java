package net.p3pp3rf1y.sophisticatedbackpacks.blocks;

import net.minecraft.block.Block;
import net.minecraft.block.BlockState;
import net.minecraft.block.SoundType;
import net.minecraft.block.material.Material;
import net.minecraft.entity.Entity;
import net.minecraft.entity.item.ItemEntity;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.entity.player.ServerPlayerEntity;
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
import net.minecraft.util.math.shapes.ISelectionContext;
import net.minecraft.util.math.shapes.VoxelShape;
import net.minecraft.util.text.ITextComponent;
import net.minecraft.world.IBlockReader;
import net.minecraft.world.World;
import net.minecraftforge.event.entity.player.PlayerInteractEvent;
import net.minecraftforge.fml.network.NetworkHooks;
import net.p3pp3rf1y.sophisticatedbackpacks.blocks.tile.BackpackTileEntity;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedbackpacks.util.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.InventoryHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.WorldHelper;

import javax.annotation.Nullable;

public class BackpackBlock extends Block {
	public static final DirectionProperty FACING = BlockStateProperties.HORIZONTAL_FACING;
	private static final VoxelShape NORTH_SHAPE = makeCuboidShape(2, 0, 5, 14, 14, 10);
	private static final VoxelShape SOUTH_SHAPE = makeCuboidShape(2, 0, 6, 14, 14, 11);
	private static final VoxelShape WEST_SHAPE = makeCuboidShape(5, 0, 2, 10, 14, 14);
	private static final VoxelShape EAST_SHAPE = makeCuboidShape(6, 0, 2, 11, 14, 14);

	public BackpackBlock() {
		super(Properties.create(Material.WOOL).notSolid().hardnessAndResistance(0.8F).sound(SoundType.CLOTH));
		setDefaultState(stateContainer.getBaseState().with(FACING, Direction.NORTH));
	}

	@Override
	protected void fillStateContainer(StateContainer.Builder<Block, BlockState> builder) {
		builder.add(FACING);
	}

	@Override
	public float getExplosionResistance() {
		return super.getExplosionResistance();
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

		NetworkHooks.openGui((ServerPlayerEntity) player, new SimpleNamedContainerProvider((w, p, pl) -> new BackpackContainer(w, pl, pos),
				getBackpackDisplayName(world, pos)), buf -> buf.writeLong(pos.toLong()));
		return ActionResultType.SUCCESS;
	}

	private ITextComponent getBackpackDisplayName(World world, BlockPos pos) {
		ITextComponent defaultDisplayName = new ItemStack(ModItems.BACKPACK.get()).getDisplayName();
		return WorldHelper.getTile(world, pos, BackpackTileEntity.class).map(te -> te.getBackpackWrapper().map(w -> w.getBackpack().getDisplayName())
				.orElse(defaultDisplayName)).orElse(defaultDisplayName);
	}

	private static void putInPlayersHandAndRemove(BlockState state, World world, BlockPos pos, PlayerEntity player, Hand hand) {
		player.setHeldItem(hand, WorldHelper.getTile(world, pos, BackpackTileEntity.class)
				.map(te -> te.getBackpackWrapper().map(IBackpackWrapper::getBackpack).orElse(ItemStack.EMPTY)).orElse(ItemStack.EMPTY));
		world.removeBlock(pos, false);

		SoundType soundType = state.getSoundType();
		world.playSound(null, pos, soundType.getBreakSound(), SoundCategory.BLOCKS, (soundType.getVolume() + 1.0F) / 2.0F, soundType.getPitch() * 0.8F);
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
			WorldHelper.getTile(world, pos, BackpackTileEntity.class).flatMap(BackpackTileEntity::getBackpackWrapper).ifPresent(w -> tryToPickup(world, itemEntity, w));
		}
	}

	private void tryToPickup(World world, ItemEntity itemEntity, IBackpackWrapper w) {
		ItemStack remainingStack = itemEntity.getItem().copy();
		InventoryHelper.runPickupOnBackpack(world, remainingStack, w, false);
		if (remainingStack.isEmpty()) {
			itemEntity.remove();
		} else if (remainingStack.getCount() < itemEntity.getItem().getCount()) {
			itemEntity.getItem().setCount(remainingStack.getCount());
		}
	}
}
