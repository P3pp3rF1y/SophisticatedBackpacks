package net.p3pp3rf1y.sophisticatedbackpacks.backpack;

import com.google.common.collect.ImmutableList;
import net.minecraft.block.BlockState;
import net.minecraft.block.SoundType;
import net.minecraft.client.gui.screen.Screen;
import net.minecraft.client.util.ITooltipFlag;
import net.minecraft.entity.Entity;
import net.minecraft.entity.LivingEntity;
import net.minecraft.entity.item.ItemEntity;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraft.fluid.FluidState;
import net.minecraft.fluid.Fluids;
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
import net.minecraft.util.math.vector.Vector3d;
import net.minecraft.util.text.ITextComponent;
import net.minecraft.util.text.StringTextComponent;
import net.minecraft.util.text.TextFormatting;
import net.minecraft.util.text.TranslationTextComponent;
import net.minecraft.world.World;
import net.minecraft.world.server.ServerWorld;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import net.minecraftforge.common.capabilities.Capability;
import net.minecraftforge.common.capabilities.ICapabilityProvider;
import net.minecraftforge.common.util.LazyOptional;
import net.minecraftforge.energy.CapabilityEnergy;
import net.minecraftforge.fluids.capability.CapabilityFluidHandler;
import net.minecraftforge.fml.common.ObfuscationReflectionHelper;
import net.minecraftforge.fml.network.NetworkHooks;
import net.minecraftforge.items.CapabilityItemHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.ITickableUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContext;
import net.p3pp3rf1y.sophisticatedbackpacks.crafting.BackpackDyeRecipe;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.everlasting.EverlastingBackpackItemEntity;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.everlasting.EverlastingUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.jukebox.ServerBackpackSoundHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.util.InventoryInteractionHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.ItemBase;
import net.p3pp3rf1y.sophisticatedbackpacks.util.PlayerInventoryProvider;
import net.p3pp3rf1y.sophisticatedbackpacks.util.WorldHelper;

import javax.annotation.Nullable;
import java.util.List;
import java.util.function.IntSupplier;
import java.util.function.Supplier;

import static net.minecraft.state.properties.BlockStateProperties.WATERLOGGED;

public class BackpackItem extends ItemBase {
	public static final String BACKPACK_TOOLTIP = "item.sophisticatedbackpacks.backpack.tooltip.";
	private final IntSupplier numberOfSlots;
	private final IntSupplier numberOfUpgradeSlots;
	private final Supplier<BackpackBlock> blockSupplier;

	public BackpackItem(IntSupplier numberOfSlots, IntSupplier numberOfUpgradeSlots, Supplier<BackpackBlock> blockSupplier) {
		this(numberOfSlots, numberOfUpgradeSlots, blockSupplier, new Properties());
	}

	public BackpackItem(IntSupplier numberOfSlots, IntSupplier numberOfUpgradeSlots, Supplier<BackpackBlock> blockSupplier, Properties properties) {
		super(properties.stacksTo(1));
		this.numberOfSlots = numberOfSlots;
		this.numberOfUpgradeSlots = numberOfUpgradeSlots;
		this.blockSupplier = blockSupplier;
	}

	@Override
	public void fillItemCategory(ItemGroup group, NonNullList<ItemStack> items) {
		super.fillItemCategory(group, items);

		if (!allowdedIn(group) || this != ModItems.BACKPACK.get() || !Config.COMMON.enabledItems.isItemEnabled(this)) {
			return;
		}

		for (DyeColor color : DyeColor.values()) {
			ItemStack stack = new ItemStack(this);
			new BackpackWrapper(stack).setColors(color.getColorValue(), color.getColorValue());
			items.add(stack);
		}

		int clothColor = BackpackDyeRecipe.calculateColor(BackpackWrapper.DEFAULT_CLOTH_COLOR, BackpackWrapper.DEFAULT_CLOTH_COLOR, ImmutableList.of(
				DyeColor.BLUE, DyeColor.YELLOW, DyeColor.LIME
		));
		int trimColor = BackpackDyeRecipe.calculateColor(BackpackWrapper.DEFAULT_BORDER_COLOR, BackpackWrapper.DEFAULT_BORDER_COLOR, ImmutableList.of(
				DyeColor.BLUE, DyeColor.BLACK
		));

		ItemStack stack = new ItemStack(this);
		new BackpackWrapper(stack).setColors(clothColor, trimColor);
		items.add(stack);
	}

	@OnlyIn(Dist.CLIENT)
	@Override
	public void appendHoverText(ItemStack stack, @Nullable World worldIn, List<ITextComponent> tooltip, ITooltipFlag flagIn) {
		super.appendHoverText(stack, worldIn, tooltip, flagIn);
		if (flagIn == ITooltipFlag.TooltipFlags.ADVANCED) {
			stack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance())
					.ifPresent(w -> w.getContentsUuid().ifPresent(uuid -> tooltip.add(new StringTextComponent("UUID: " + uuid).withStyle(TextFormatting.DARK_GRAY))));
		}
		if (!Screen.hasShiftDown()) {
			tooltip.add(new TranslationTextComponent(
					BACKPACK_TOOLTIP + "press_for_contents",
					new TranslationTextComponent(BACKPACK_TOOLTIP + "shift").withStyle(TextFormatting.AQUA)
			).withStyle(TextFormatting.GRAY));
		}
	}

	@Override
	public boolean hasCustomEntity(ItemStack stack) {
		return hasEverlastingUpgrade(stack);
	}

	private boolean hasEverlastingUpgrade(ItemStack stack) {
		return stack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance()).map(w -> !w.getUpgradeHandler().getTypeWrappers(EverlastingUpgradeItem.TYPE).isEmpty()).orElse(false);
	}

	@Nullable
	@Override
	public Entity createEntity(World world, Entity entity, ItemStack itemstack) {
		if (!(entity instanceof ItemEntity)) {
			return null;
		}
		return hasEverlastingUpgrade(itemstack) ? createEverlastingBackpack(world, (ItemEntity) entity, itemstack) : null;
	}

	@Nullable
	private EverlastingBackpackItemEntity createEverlastingBackpack(World world, ItemEntity itemEntity, ItemStack itemstack) {
		EverlastingBackpackItemEntity backpackItemEntity = ModItems.EVERLASTING_BACKPACK_ITEM_ENTITY.get().create(world);
		if (backpackItemEntity != null) {
			backpackItemEntity.setPos(itemEntity.getX(), itemEntity.getY(), itemEntity.getZ());
			backpackItemEntity.setItem(itemstack);
			backpackItemEntity.setPickUpDelay(getPickupDelay(itemEntity));
			backpackItemEntity.setThrower(itemEntity.getThrower());
			backpackItemEntity.setDeltaMovement(itemEntity.getDeltaMovement());
		}
		return backpackItemEntity;
	}

	private int getPickupDelay(ItemEntity itemEntity) {
		Integer result = ObfuscationReflectionHelper.getPrivateValue(ItemEntity.class, itemEntity, "field_145804_b");
		if (result == null) {
			SophisticatedBackpacks.LOGGER.error("Reflection get of pickupDelay (pickupDelay) from ItemEntity returned null");
			return 20;
		}
		return result;
	}

	@Override
	public ActionResultType useOn(ItemUseContext context) {
		PlayerEntity player = context.getPlayer();
		if (player == null || !player.isShiftKeyDown()) {
			return ActionResultType.PASS;
		}

		if (InventoryInteractionHelper.tryInventoryInteraction(context)) {
			return ActionResultType.SUCCESS;
		}

		Direction direction = player.getDirection().getOpposite();

		BlockItemUseContext blockItemUseContext = new BlockItemUseContext(context);
		ActionResultType result = tryPlace(player, direction, blockItemUseContext);
		return result == ActionResultType.PASS ? super.useOn(context) : result;
	}

	public ActionResultType tryPlace(@Nullable PlayerEntity player, Direction direction, BlockItemUseContext blockItemUseContext) {
		if (!blockItemUseContext.canPlace()) {
			return ActionResultType.FAIL;
		}
		World world = blockItemUseContext.getLevel();
		BlockPos pos = blockItemUseContext.getClickedPos();

		FluidState fluidstate = blockItemUseContext.getLevel().getFluidState(pos);
		BlockState placementState = blockSupplier.get().defaultBlockState().setValue(BackpackBlock.FACING, direction)
				.setValue(WATERLOGGED, fluidstate.getType() == Fluids.WATER);
		if (!canPlace(blockItemUseContext, placementState)) {
			return ActionResultType.FAIL;
		}

		if (world.setBlockAndUpdate(pos, placementState)) {
			ItemStack backpack = blockItemUseContext.getItemInHand();
			WorldHelper.getTile(world, pos, BackpackTileEntity.class).ifPresent(te -> {
				te.setBackpack(getBackpackCopy(player, backpack));
				te.refreshRenderState();
			});

			if (!world.isClientSide) {
				stopBackpackSounds(backpack, world, pos);
			}

			SoundType soundtype = placementState.getSoundType(world, pos, player);
			world.playSound(player, pos, soundtype.getPlaceSound(), SoundCategory.BLOCKS, (soundtype.getVolume() + 1.0F) / 2.0F, soundtype.getPitch() * 0.8F);
			if (player == null || !player.isCreative()) {
				backpack.shrink(1);
			}

			return ActionResultType.SUCCESS;
		}
		return ActionResultType.PASS;
	}

	private static void stopBackpackSounds(ItemStack backpack, World world, BlockPos pos) {
		backpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance()).ifPresent(wrapper -> wrapper.getContentsUuid().ifPresent(uuid ->
				ServerBackpackSoundHandler.stopPlayingDisc((ServerWorld) world, Vector3d.atCenterOf(pos), uuid))
		);
	}

	private ItemStack getBackpackCopy(@Nullable PlayerEntity player, ItemStack backpack) {
		if (player == null || !player.isCreative()) {
			return backpack.copy();
		}
		return backpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance())
				.map(IBackpackWrapper::cloneBackpack).orElse(new ItemStack(ModItems.BACKPACK.get()));
	}

	protected boolean canPlace(BlockItemUseContext context, BlockState state) {
		PlayerEntity playerentity = context.getPlayer();
		ISelectionContext iselectioncontext = playerentity == null ? ISelectionContext.empty() : ISelectionContext.of(playerentity);
		return (state.canSurvive(context.getLevel(), context.getClickedPos())) && context.getLevel().isUnobstructed(state, context.getClickedPos(), iselectioncontext);
	}

	@Override
	public ActionResult<ItemStack> use(World world, PlayerEntity player, Hand hand) {
		ItemStack stack = player.getItemInHand(hand);

		if (!world.isClientSide && player instanceof ServerPlayerEntity) {
			String handlerName = hand == Hand.MAIN_HAND ? PlayerInventoryProvider.MAIN_INVENTORY : PlayerInventoryProvider.OFFHAND_INVENTORY;
			int slot = hand == Hand.MAIN_HAND ? player.inventory.selected : 0;
			BackpackContext.Item context = new BackpackContext.Item(handlerName, slot);
			NetworkHooks.openGui((ServerPlayerEntity) player, new SimpleNamedContainerProvider((w, p, pl) -> new BackpackContainer(w, pl, context), stack.getHoverName()),
					context::toBuffer);
		}
		return ActionResult.success(stack);
	}

	@Override
	public ICapabilityProvider initCapabilities(ItemStack stack, @Nullable CompoundNBT nbt) {
		return new ICapabilityProvider() {
			private IBackpackWrapper wrapper = null;

			@Override
			public <T> LazyOptional<T> getCapability(Capability<T> cap, @Nullable Direction side) {
				initWrapper();
				if (cap == CapabilityBackpackWrapper.getCapabilityInstance()) {
					return LazyOptional.of(() -> wrapper).cast();
				} else if (cap == CapabilityItemHandler.ITEM_HANDLER_CAPABILITY) {
					return LazyOptional.of(() -> wrapper.getInventoryForInputOutput()).cast();
				} else if (cap == CapabilityFluidHandler.FLUID_HANDLER_ITEM_CAPABILITY) {
					return wrapper.getFluidHandler().<LazyOptional<T>>map(handler -> LazyOptional.of(() -> handler).cast()).orElseGet(LazyOptional::empty);
				} else if (cap == CapabilityEnergy.ENERGY) {
					return wrapper.getEnergyStorage().<LazyOptional<T>>map(storage -> LazyOptional.of(() -> storage).cast()).orElseGet(LazyOptional::empty);
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
		if (worldIn.isClientSide || !(entityIn instanceof PlayerEntity)) {
			return;
		}
		PlayerEntity player = (PlayerEntity) entityIn;
		stack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance()).ifPresent(
				wrapper -> wrapper.getUpgradeHandler().getWrappersThatImplement(ITickableUpgrade.class)
						.forEach(upgrade -> upgrade.tick(player, player.level, player.blockPosition()))
		);
		super.inventoryTick(stack, worldIn, entityIn, itemSlot, isSelected);
	}

	public int getNumberOfSlots() {
		return numberOfSlots.getAsInt();
	}

	public int getNumberOfUpgradeSlots() {
		return numberOfUpgradeSlots.getAsInt();
	}

	@Nullable
	@Override
	public EquipmentSlotType getEquipmentSlot(ItemStack stack) {
		return EquipmentSlotType.CHEST;
	}

	@Override
	public boolean shouldCauseReequipAnimation(ItemStack oldStack, ItemStack newStack, boolean slotChanged) {
		return slotChanged;
	}

	@Override
	public boolean makesPiglinsNeutral(ItemStack stack, LivingEntity wearer) {
		return stack.getItem() == ModItems.GOLD_BACKPACK.get();
	}
}