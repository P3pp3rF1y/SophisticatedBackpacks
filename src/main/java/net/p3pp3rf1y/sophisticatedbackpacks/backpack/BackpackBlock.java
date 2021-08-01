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
import net.minecraft.state.BooleanProperty;
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
import net.minecraft.util.math.vector.Vector3d;
import net.minecraft.util.text.ITextComponent;
import net.minecraft.world.Explosion;
import net.minecraft.world.IBlockReader;
import net.minecraft.world.IWorld;
import net.minecraft.world.World;
import net.minecraft.world.server.ServerWorld;
import net.minecraftforge.event.entity.player.PlayerInteractEvent;
import net.minecraftforge.fluids.FluidActionResult;
import net.minecraftforge.fluids.FluidAttributes;
import net.minecraftforge.fluids.FluidUtil;
import net.minecraftforge.fluids.capability.CapabilityFluidHandler;
import net.minecraftforge.fluids.capability.IFluidHandler;
import net.minecraftforge.fml.network.NetworkHooks;
import net.minecraftforge.items.CapabilityItemHandler;
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

import static net.minecraft.state.properties.BlockStateProperties.WATERLOGGED;

public class BackpackBlock extends Block implements IWaterLoggable {
	public static final BooleanProperty LEFT_TANK = BooleanProperty.create("left_tank");
	public static final BooleanProperty RIGHT_TANK = BooleanProperty.create("right_tank");
	public static final BooleanProperty BATTERY = BooleanProperty.create("battery");

	public static final DirectionProperty FACING = BlockStateProperties.HORIZONTAL_FACING;
	private static final int BEDROCK_RESISTANCE = 3600000;

	public BackpackBlock() {
		super(Properties.of(Material.WOOL).noOcclusion().strength(0.8F).sound(SoundType.WOOL));
		registerDefaultState(stateDefinition.any().setValue(FACING, Direction.NORTH).setValue(WATERLOGGED, false).setValue(LEFT_TANK, false).setValue(RIGHT_TANK, false));
	}

	@Override
	public FluidState getFluidState(BlockState state) {
		return Boolean.TRUE.equals(state.getValue(WATERLOGGED)) ? Fluids.WATER.getSource(false) : super.getFluidState(state);
	}

	@Override
	public BlockState updateShape(BlockState stateIn, Direction facing, BlockState facingState, IWorld worldIn, BlockPos currentPos, BlockPos facingPos) {
		if (Boolean.TRUE.equals(stateIn.getValue(WATERLOGGED))) {
			worldIn.getLiquidTicks().scheduleTick(currentPos, Fluids.WATER, Fluids.WATER.getTickDelay(worldIn));
		}

		return super.updateShape(stateIn, facing, facingState, worldIn, currentPos, facingPos);
	}

	@Override
	protected void createBlockStateDefinition(StateContainer.Builder<Block, BlockState> builder) {
		builder.add(FACING, WATERLOGGED, LEFT_TANK, RIGHT_TANK, BATTERY);
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
		return BackpackShapes.getShape(state.getValue(FACING), state.getValue(LEFT_TANK), state.getValue(RIGHT_TANK), state.getValue(BATTERY));
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
	public ActionResultType use(BlockState state, World world, BlockPos pos, PlayerEntity player, Hand hand, BlockRayTraceResult hit) {
		if (world.isClientSide) {
			return ActionResultType.SUCCESS;
		}

		ItemStack heldItem = player.getItemInHand(hand);
		if (player.isShiftKeyDown() && heldItem.isEmpty()) {
			putInPlayersHandAndRemove(state, world, pos, player, hand);
			return ActionResultType.SUCCESS;
		}

		if (!heldItem.isEmpty() && heldItem.getCapability(CapabilityFluidHandler.FLUID_HANDLER_ITEM_CAPABILITY).isPresent()) {
			WorldHelper.getTile(world, pos, BackpackTileEntity.class).ifPresent(te -> {
				IFluidHandler backpackFluidHandler = te.getBackpackWrapper().getFluidHandler();
				player.getCapability(CapabilityItemHandler.ITEM_HANDLER_CAPABILITY).ifPresent(playerInventory -> {
					FluidActionResult resultOfEmptying = FluidUtil.tryEmptyContainerAndStow(heldItem, backpackFluidHandler, playerInventory, FluidAttributes.BUCKET_VOLUME, player, true);
					if (resultOfEmptying.isSuccess()) {
						player.setItemInHand(hand, resultOfEmptying.getResult());
					} else {
						FluidActionResult resultOfFilling = FluidUtil.tryFillContainerAndStow(heldItem, backpackFluidHandler, playerInventory, FluidAttributes.BUCKET_VOLUME, player, true);
						if (resultOfFilling.isSuccess()) {
							player.setItemInHand(hand, resultOfFilling.getResult());
						}
					}
				});
			});
			return ActionResultType.SUCCESS;
		}

		BackpackContext.Block backpackContext = new BackpackContext.Block(pos);
		NetworkHooks.openGui((ServerPlayerEntity) player, new SimpleNamedContainerProvider((w, p, pl) -> new BackpackContainer(w, pl, backpackContext),
				getBackpackDisplayName(world, pos)), backpackContext::toBuffer);
		return ActionResultType.SUCCESS;
	}

	private ITextComponent getBackpackDisplayName(World world, BlockPos pos) {
		ITextComponent defaultDisplayName = new ItemStack(ModItems.BACKPACK.get()).getHoverName();
		return WorldHelper.getTile(world, pos, BackpackTileEntity.class).map(te -> te.getBackpackWrapper().getBackpack().getHoverName()).orElse(defaultDisplayName);
	}

	private static void putInPlayersHandAndRemove(BlockState state, World world, BlockPos pos, PlayerEntity player, Hand hand) {
		ItemStack backpack = WorldHelper.getTile(world, pos, BackpackTileEntity.class).map(te -> te.getBackpackWrapper().getBackpack()).orElse(ItemStack.EMPTY);
		player.setItemInHand(hand, backpack);
		player.getCooldowns().addCooldown(backpack.getItem(), 5);
		world.removeBlock(pos, false);

		stopBackpackSounds(backpack, world, pos);

		SoundType soundType = state.getSoundType();
		world.playSound(null, pos, soundType.getBreakSound(), SoundCategory.BLOCKS, (soundType.getVolume() + 1.0F) / 2.0F, soundType.getPitch() * 0.8F);
	}

	private static void stopBackpackSounds(ItemStack backpack, World world, BlockPos pos) {
		backpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance()).ifPresent(wrapper -> wrapper.getContentsUuid().ifPresent(uuid ->
				ServerBackpackSoundHandler.stopPlayingDisc((ServerWorld) world, Vector3d.atCenterOf(pos), uuid))
		);
	}

	public static void playerInteract(PlayerInteractEvent.RightClickBlock event) {
		PlayerEntity player = event.getPlayer();
		World world = player.level;
		BlockPos pos = event.getPos();

		if (!player.isShiftKeyDown() || !hasEmptyMainHandAndSomethingInOffhand(player) || didntInteractWithBackpack(event)) {
			return;
		}

		if (world.isClientSide) {
			event.setCanceled(true);
			event.setCancellationResult(ActionResultType.SUCCESS);
			return;
		}

		BlockState state = world.getBlockState(pos);
		if (!(state.getBlock() instanceof BackpackBlock)) {
			return;
		}

		putInPlayersHandAndRemove(state, world, pos, player, player.getMainHandItem().isEmpty() ? Hand.MAIN_HAND : Hand.OFF_HAND);

		event.setCanceled(true);
		event.setCancellationResult(ActionResultType.SUCCESS);
	}

	private static boolean didntInteractWithBackpack(PlayerInteractEvent.RightClickBlock event) {
		return !(event.getWorld().getBlockState(event.getPos()).getBlock() instanceof BackpackBlock);
	}

	private static boolean hasEmptyMainHandAndSomethingInOffhand(PlayerEntity player) {
		return player.getMainHandItem().isEmpty() && !player.getOffhandItem().isEmpty();
	}

	@Override
	public void entityInside(BlockState state, World world, BlockPos pos, Entity entity) {
		super.entityInside(state, world, pos, entity);
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