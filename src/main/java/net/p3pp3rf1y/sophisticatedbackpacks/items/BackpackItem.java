package net.p3pp3rf1y.sophisticatedbackpacks.items;

import net.minecraft.block.BlockState;
import net.minecraft.block.SoundType;
import net.minecraft.entity.Entity;
import net.minecraft.entity.item.ItemEntity;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraft.inventory.EquipmentSlotType;
import net.minecraft.inventory.container.SimpleNamedContainerProvider;
import net.minecraft.item.BlockItemUseContext;
import net.minecraft.item.DyeColor;
import net.minecraft.item.ItemGroup;
import net.minecraft.item.ItemStack;
import net.minecraft.item.ItemUseContext;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.util.ActionResult;
import net.minecraft.util.ActionResultType;
import net.minecraft.util.Direction;
import net.minecraft.util.Hand;
import net.minecraft.util.NonNullList;
import net.minecraft.util.SoundCategory;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.shapes.ISelectionContext;
import net.minecraft.world.World;
import net.minecraftforge.common.capabilities.Capability;
import net.minecraftforge.common.capabilities.ICapabilityProvider;
import net.minecraftforge.common.util.LazyOptional;
import net.minecraftforge.fml.common.ObfuscationReflectionHelper;
import net.minecraftforge.fml.network.NetworkHooks;
import net.minecraftforge.items.CapabilityItemHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IItemHandlerInteractionUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.api.ITickableUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.blocks.BackpackBlock;
import net.p3pp3rf1y.sophisticatedbackpacks.blocks.tile.BackpackTileEntity;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.everlasting.EverlastingBackpackItemEntity;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.everlasting.EverlastingUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.util.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.PlayerInventoryProvider;
import net.p3pp3rf1y.sophisticatedbackpacks.util.WorldHelper;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.List;
import java.util.function.Supplier;

public class BackpackItem extends ItemBase {
	private final int numberOfSlots;
	private final int numberOfUpgradeSlots;
	private final ScreenProperties screenProperties;
	private final Supplier<BackpackBlock> blockSupplier;

	public BackpackItem(int numberOfSlots, int numberOfUpgradeSlots, Supplier<BackpackBlock> blockSupplier) {
		this(numberOfSlots, numberOfUpgradeSlots, new ScreenProperties(), blockSupplier);
	}

	public BackpackItem(int numberOfSlots, int numberOfUpgradeSlots, ScreenProperties screenProperties, Supplier<BackpackBlock> blockSupplier) {
		super(new Properties().maxStackSize(1));
		this.numberOfSlots = numberOfSlots;
		this.numberOfUpgradeSlots = numberOfUpgradeSlots;
		this.screenProperties = screenProperties;
		this.blockSupplier = blockSupplier;
	}

	@Override
	public void fillItemGroup(ItemGroup group, NonNullList<ItemStack> items) {
		super.fillItemGroup(group, items);

		if (!isInGroup(group) || this != ModItems.BACKPACK.get()) {
			return;
		}

		for (DyeColor color : DyeColor.values()) {
			ItemStack stack = new ItemStack(this);
			stack.getCapability(BackpackWrapper.BACKPACK_WRAPPER_CAPABILITY)
					.ifPresent(wrapper -> wrapper.setColors(color.getColorValue(), color.getColorValue()));
			items.add(stack);
		}

		ItemStack stack = new ItemStack(this);
		stack.getCapability(BackpackWrapper.BACKPACK_WRAPPER_CAPABILITY)
				.ifPresent(wrapper -> wrapper.setColors(DyeColor.YELLOW.getColorValue(), DyeColor.BLUE.getColorValue()));
		items.add(stack);
	}

	@Override
	public boolean hasCustomEntity(ItemStack stack) {
		return hasEverlastingUpgrade(stack);
	}

	private boolean hasEverlastingUpgrade(ItemStack stack) {
		return stack.getCapability(BackpackWrapper.BACKPACK_WRAPPER_CAPABILITY).map(w -> !w.getUpgradeHandler().getTypeWrappers(EverlastingUpgradeItem.TYPE).isEmpty()).orElse(false);
	}

	@Nullable
	@Override
	public Entity createEntity(World world, Entity entity, ItemStack itemstack) {
		if (!(entity instanceof ItemEntity)) {
			return null;
		}
		return hasEverlastingUpgrade(itemstack) ? createEverlastingBackpack(world, (ItemEntity) entity, itemstack) : null;
	}

	private EverlastingBackpackItemEntity createEverlastingBackpack(World world, ItemEntity itemEntity, ItemStack itemstack) {
		EverlastingBackpackItemEntity backpackItemEntity = ModItems.EVERLASTING_BACKPACK_ITEM_ENTITY.get().create(world);
		backpackItemEntity.setPosition(itemEntity.getPosX(), itemEntity.getPosY(), itemEntity.getPosZ());
		backpackItemEntity.setItem(itemstack);
		backpackItemEntity.setPickupDelay(ObfuscationReflectionHelper.getPrivateValue(ItemEntity.class, itemEntity, "field_145804_b"));
		backpackItemEntity.setThrowerId(itemEntity.getThrowerId());
		backpackItemEntity.setMotion(itemEntity.getMotion());
		return backpackItemEntity;
	}

	@Override
	public ActionResultType onItemUse(ItemUseContext context) {
		PlayerEntity player = context.getPlayer();
		if (player == null || !player.isSneaking()) {
			return ActionResultType.PASS;
		}

		if (tryInventoryInteraction(context)) {
			return ActionResultType.SUCCESS;
		}

		BlockItemUseContext blockItemUseContext = new BlockItemUseContext(context);
		if (!blockItemUseContext.canPlace()) {
			return ActionResultType.FAIL;
		}
		World world = blockItemUseContext.getWorld();
		BlockPos pos = blockItemUseContext.getPos();

		BlockState placementState = blockSupplier.get().getDefaultState().with(BackpackBlock.FACING, player.getHorizontalFacing().getOpposite());
		if (!canPlace(blockItemUseContext, placementState)) {
			return ActionResultType.FAIL;
		}

		if (world.setBlockState(pos, placementState, 11)) {
			ItemStack backpack = blockItemUseContext.getItem();
			WorldHelper.getTile(world, pos, BackpackTileEntity.class).ifPresent(te -> te.setBackpack(backpack.copy()));

			SoundType soundtype = placementState.getSoundType(world, pos, player);
			world.playSound(player, pos, soundtype.getPlaceSound(), SoundCategory.BLOCKS, (soundtype.getVolume() + 1.0F) / 2.0F, soundtype.getPitch() * 0.8F);
			if (!player.abilities.isCreativeMode) {
				backpack.shrink(1);
			}

			return ActionResultType.SUCCESS;
		}
		return super.onItemUse(context);
	}

	private boolean tryInventoryInteraction(ItemUseContext context) {
		return WorldHelper.getTile(context.getWorld(), context.getPos())
				.map(te -> te.getCapability(CapabilityItemHandler.ITEM_HANDLER_CAPABILITY, context.getFace())
						.map(itemHandler -> {
							ItemStack backpack = context.getItem();
							return backpack.getCapability(BackpackWrapper.BACKPACK_WRAPPER_CAPABILITY)
									.map(wrapper -> tryRunningInteractionWrappers(itemHandler, wrapper))
									.orElse(false);
						}).orElse(false)
				).orElse(false);
	}

	@Nonnull
	private Boolean tryRunningInteractionWrappers(net.minecraftforge.items.IItemHandler itemHandler, IBackpackWrapper wrapper) {
		List<IItemHandlerInteractionUpgrade> wrappers = wrapper.getUpgradeHandler().getWrappersThatImplement(IItemHandlerInteractionUpgrade.class);
		if (wrappers.isEmpty()) {
			return false;
		}
		wrappers.forEach(upgrade -> upgrade.onHandlerInteract(wrapper, itemHandler));
		return true;
	}

	protected boolean canPlace(BlockItemUseContext context, BlockState state) {
		PlayerEntity playerentity = context.getPlayer();
		ISelectionContext iselectioncontext = playerentity == null ? ISelectionContext.dummy() : ISelectionContext.forEntity(playerentity);
		return (state.isValidPosition(context.getWorld(), context.getPos())) && context.getWorld().placedBlockCollides(state, context.getPos(), iselectioncontext);
	}

	@Override
	public ActionResult<ItemStack> onItemRightClick(World world, PlayerEntity player, Hand hand) {
		ItemStack stack = player.getHeldItem(hand);

		if (!world.isRemote && player instanceof ServerPlayerEntity) {
			String handlerName = hand == Hand.MAIN_HAND ? PlayerInventoryProvider.MAIN_INVENTORY : PlayerInventoryProvider.OFFHAND_INVENTORY;
			int slot = hand == Hand.MAIN_HAND ? player.inventory.currentItem : 0;
			NetworkHooks.openGui((ServerPlayerEntity) player, new SimpleNamedContainerProvider((w, p, pl) -> new BackpackContainer(w, pl, handlerName, slot), stack.getDisplayName()),
					buf -> {
						buf.writeString(handlerName);
						buf.writeInt(slot);
					});
		}
		return ActionResult.resultSuccess(stack);
	}

	@Override
	public ICapabilityProvider initCapabilities(ItemStack stack, @Nullable CompoundNBT nbt) {
		return new ICapabilityProvider() {
			private IBackpackWrapper wrapper = null;

			@Override
			public <T> LazyOptional<T> getCapability(Capability<T> cap, @Nullable Direction side) {
				initWrapper();
				if (cap == BackpackWrapper.BACKPACK_WRAPPER_CAPABILITY) {
					return LazyOptional.of(() -> wrapper).cast();
				} else if (cap == CapabilityItemHandler.ITEM_HANDLER_CAPABILITY) {
					return LazyOptional.of(() -> wrapper.getFilteredHandler()).cast();
				}
				return LazyOptional.empty();
			}

			private void initWrapper() {
				if (wrapper == null) {
					wrapper = new BackpackWrapper(stack);
				}
			}
		};
	}

	@Override
	public void inventoryTick(ItemStack stack, World worldIn, Entity entityIn, int itemSlot, boolean isSelected) {
		if (worldIn.isRemote || !(entityIn instanceof PlayerEntity)) {
			return;
		}
		PlayerEntity player = (PlayerEntity) entityIn;
		stack.getCapability(BackpackWrapper.BACKPACK_WRAPPER_CAPABILITY).ifPresent(
				wrapper -> wrapper.getUpgradeHandler().getWrappersThatImplement(ITickableUpgrade.class)
						.forEach(upgrade -> upgrade.tick(player, player.world, player.getPosition(), wrapper))
		);
		super.inventoryTick(stack, worldIn, entityIn, itemSlot, isSelected);
	}

	public ScreenProperties getScreenProperties() {
		return screenProperties;
	}

	public int getNumberOfSlots() {
		return numberOfSlots;
	}

	public int getNumberOfUpgradeSlots() {
		return numberOfUpgradeSlots;
	}

	@Nullable
	@Override
	public EquipmentSlotType getEquipmentSlot(ItemStack stack) {
		return EquipmentSlotType.CHEST;
	}
}