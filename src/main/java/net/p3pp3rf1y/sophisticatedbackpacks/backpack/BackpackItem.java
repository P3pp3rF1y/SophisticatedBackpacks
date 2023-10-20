package net.p3pp3rf1y.sophisticatedbackpacks.backpack;

import net.minecraft.ChatFormatting;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.screens.Screen;
import net.minecraft.client.renderer.BlockEntityWithoutLevelRenderer;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.core.NonNullList;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TextComponent;
import net.minecraft.network.chat.TranslatableComponent;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.sounds.SoundSource;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.InteractionResultHolder;
import net.minecraft.world.SimpleMenuProvider;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EquipmentSlot;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.SlotAccess;
import net.minecraft.world.entity.item.ItemEntity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.ClickAction;
import net.minecraft.world.inventory.Slot;
import net.minecraft.world.inventory.tooltip.TooltipComponent;
import net.minecraft.world.item.CreativeModeTab;
import net.minecraft.world.item.DyeColor;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.TooltipFlag;
import net.minecraft.world.item.context.BlockPlaceContext;
import net.minecraft.world.item.context.UseOnContext;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.SoundType;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.material.FluidState;
import net.minecraft.world.level.material.Fluids;
import net.minecraft.world.phys.Vec3;
import net.minecraft.world.phys.shapes.CollisionContext;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.client.IItemRenderProperties;
import net.minecraftforge.common.capabilities.Capability;
import net.minecraftforge.common.capabilities.ICapabilityProvider;
import net.minecraftforge.common.util.LazyOptional;
import net.minecraftforge.common.util.NonNullLazy;
import net.minecraftforge.energy.CapabilityEnergy;
import net.minecraftforge.fluids.capability.CapabilityFluidHandler;
import net.minecraftforge.fml.DistExecutor;
import net.minecraftforge.fml.util.ObfuscationReflectionHelper;
import net.minecraftforge.items.CapabilityItemHandler;
import net.minecraftforge.network.NetworkHooks;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.render.BackpackItemStackRenderer;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContext;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.everlasting.EverlastingBackpackItemEntity;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.everlasting.EverlastingUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.util.InventoryInteractionHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.PlayerInventoryProvider;
import net.p3pp3rf1y.sophisticatedcore.api.IStashStorageItem;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.TranslationHelper;
import net.p3pp3rf1y.sophisticatedcore.settings.memory.MemorySettingsCategory;
import net.p3pp3rf1y.sophisticatedcore.upgrades.ITickableUpgrade;
import net.p3pp3rf1y.sophisticatedcore.upgrades.jukebox.ServerStorageSoundHandler;
import net.p3pp3rf1y.sophisticatedcore.util.ColorHelper;
import net.p3pp3rf1y.sophisticatedcore.util.ItemBase;
import net.p3pp3rf1y.sophisticatedcore.util.WorldHelper;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;
import java.util.function.IntSupplier;
import java.util.function.Supplier;
import java.util.function.UnaryOperator;

import static net.minecraft.world.level.block.state.properties.BlockStateProperties.WATERLOGGED;

public class BackpackItem extends ItemBase implements IStashStorageItem {
	private final IntSupplier numberOfSlots;
	private final IntSupplier numberOfUpgradeSlots;
	private final Supplier<BackpackBlock> blockSupplier;

	public BackpackItem(IntSupplier numberOfSlots, IntSupplier numberOfUpgradeSlots, Supplier<BackpackBlock> blockSupplier) {
		this(numberOfSlots, numberOfUpgradeSlots, blockSupplier, p -> p);
	}

	public BackpackItem(IntSupplier numberOfSlots, IntSupplier numberOfUpgradeSlots, Supplier<BackpackBlock> blockSupplier, UnaryOperator<Properties> updateProperties) {
		super(updateProperties.apply(new Properties().stacksTo(1)), SophisticatedBackpacks.ITEM_GROUP);
		this.numberOfSlots = numberOfSlots;
		this.numberOfUpgradeSlots = numberOfUpgradeSlots;
		this.blockSupplier = blockSupplier;
	}

	@Override
	public void initializeClient(Consumer<IItemRenderProperties> consumer) {
		consumer.accept(new IItemRenderProperties() {
			private final NonNullLazy<BlockEntityWithoutLevelRenderer> ister = NonNullLazy.of(() -> new BackpackItemStackRenderer(Minecraft.getInstance().getBlockEntityRenderDispatcher(), Minecraft.getInstance().getEntityModels()));

			@Override
			public BlockEntityWithoutLevelRenderer getItemStackRenderer() {
				return ister.get();
			}
		});
	}

	@Override
	public void fillItemCategory(CreativeModeTab group, NonNullList<ItemStack> items) {
		super.fillItemCategory(group, items);

		if (!allowdedIn(group) || this != ModItems.BACKPACK.get() || !net.p3pp3rf1y.sophisticatedcore.Config.COMMON.enabledItems.isItemEnabled(this)) {
			return;
		}

		for (DyeColor color : DyeColor.values()) {
			ItemStack stack = new ItemStack(this);
			new BackpackWrapper(stack).setColors(ColorHelper.getColor(color.getTextureDiffuseColors()), ColorHelper.getColor(color.getTextureDiffuseColors()));
			items.add(stack);
		}

		int clothColor = ColorHelper.calculateColor(BackpackWrapper.DEFAULT_CLOTH_COLOR, BackpackWrapper.DEFAULT_CLOTH_COLOR, List.of(
				DyeColor.BLUE, DyeColor.YELLOW, DyeColor.LIME
		));
		int trimColor = ColorHelper.calculateColor(BackpackWrapper.DEFAULT_BORDER_COLOR, BackpackWrapper.DEFAULT_BORDER_COLOR, List.of(
				DyeColor.BLUE, DyeColor.BLACK
		));

		ItemStack stack = new ItemStack(this);
		new BackpackWrapper(stack).setColors(clothColor, trimColor);
		items.add(stack);
	}

	@Override
	public void appendHoverText(ItemStack stack, @Nullable Level worldIn, List<Component> tooltip, TooltipFlag flagIn) {
		super.appendHoverText(stack, worldIn, tooltip, flagIn);
		if (flagIn == TooltipFlag.Default.ADVANCED) {
			stack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance())
					.ifPresent(w -> w.getContentsUuid().ifPresent(uuid -> tooltip.add(new TextComponent("UUID: " + uuid).withStyle(ChatFormatting.DARK_GRAY))));
		}
		if (!Screen.hasShiftDown()) {
			tooltip.add(new TranslatableComponent(
					TranslationHelper.INSTANCE.translItemTooltip("storage") + ".press_for_contents",
					new TranslatableComponent(TranslationHelper.INSTANCE.translItemTooltip("storage") + ".shift").withStyle(ChatFormatting.AQUA)
			).withStyle(ChatFormatting.GRAY));
		}
	}

	@Override
	public Optional<TooltipComponent> getTooltipImage(ItemStack stack) {
		AtomicReference<TooltipComponent> ret = new AtomicReference<>(null);
		DistExecutor.unsafeRunWhenOn(Dist.CLIENT, () -> () -> {
			Minecraft mc = Minecraft.getInstance();
			if (Screen.hasShiftDown() || (mc.player != null && !mc.player.containerMenu.getCarried().isEmpty())) {
				ret.set(new BackpackContentsTooltip(stack));
			}
		});
		return Optional.ofNullable(ret.get());
	}

	@Override
	public boolean hasCustomEntity(ItemStack stack) {
		return true;
	}

	private boolean hasEverlastingUpgrade(ItemStack stack) {
		return stack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance()).map(w -> !w.getUpgradeHandler().getTypeWrappers(EverlastingUpgradeItem.TYPE).isEmpty()).orElse(false);
	}

	@Nullable
	@Override
	public Entity createEntity(Level world, Entity entity, ItemStack itemstack) {
		if (!(entity instanceof ItemEntity itemEntity)) {
			return null;
		}

		UUIDDeduplicator.dedupeBackpackItemEntityInArea(itemEntity);

		return hasEverlastingUpgrade(itemstack) ? createEverlastingBackpack(world, (ItemEntity) entity, itemstack) : null;
	}

	@Nullable
	private EverlastingBackpackItemEntity createEverlastingBackpack(Level world, ItemEntity itemEntity, ItemStack itemstack) {
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
		Integer result = ObfuscationReflectionHelper.getPrivateValue(ItemEntity.class, itemEntity, "f_31986_");
		if (result == null) {
			SophisticatedBackpacks.LOGGER.error("Reflection get of pickupDelay (pickupDelay) from ItemEntity returned null");
			return 20;
		}
		return result;
	}

	@Override
	public InteractionResult useOn(UseOnContext context) {
		Player player = context.getPlayer();
		if (player == null || !player.isShiftKeyDown()) {
			return InteractionResult.PASS;
		}

		if (InventoryInteractionHelper.tryInventoryInteraction(context)) {
			return InteractionResult.SUCCESS;
		}

		Direction direction = player.getDirection().getOpposite();

		BlockPlaceContext blockItemUseContext = new BlockPlaceContext(context);
		InteractionResult result = tryPlace(player, direction, blockItemUseContext);
		return result == InteractionResult.PASS ? super.useOn(context) : result;
	}

	public InteractionResult tryPlace(@Nullable Player player, Direction direction, BlockPlaceContext blockItemUseContext) {
		if (!blockItemUseContext.canPlace()) {
			return InteractionResult.FAIL;
		}
		Level world = blockItemUseContext.getLevel();
		BlockPos pos = blockItemUseContext.getClickedPos();

		FluidState fluidstate = blockItemUseContext.getLevel().getFluidState(pos);
		BlockState placementState = blockSupplier.get().defaultBlockState().setValue(BackpackBlock.FACING, direction)
				.setValue(WATERLOGGED, fluidstate.getType() == Fluids.WATER);
		if (!canPlace(blockItemUseContext, placementState)) {
			return InteractionResult.FAIL;
		}

		if (world.setBlockAndUpdate(pos, placementState)) {
			ItemStack backpack = blockItemUseContext.getItemInHand();
			WorldHelper.getBlockEntity(world, pos, BackpackBlockEntity.class).ifPresent(te -> {
				te.setBackpack(getBackpackCopy(player, backpack));
				te.refreshRenderState();

				te.tryToAddToController();
			});

			if (!world.isClientSide) {
				stopBackpackSounds(backpack, world, pos);
			}

			SoundType soundtype = placementState.getSoundType(world, pos, player);
			world.playSound(player, pos, soundtype.getPlaceSound(), SoundSource.BLOCKS, (soundtype.getVolume() + 1.0F) / 2.0F, soundtype.getPitch() * 0.8F);
			if (player == null || !player.isCreative()) {
				backpack.shrink(1);
			}

			return InteractionResult.SUCCESS;
		}
		return InteractionResult.PASS;
	}

	private static void stopBackpackSounds(ItemStack backpack, Level world, BlockPos pos) {
		backpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance()).ifPresent(wrapper -> wrapper.getContentsUuid().ifPresent(uuid ->
				ServerStorageSoundHandler.stopPlayingDisc((ServerLevel) world, Vec3.atCenterOf(pos), uuid))
		);
	}

	private ItemStack getBackpackCopy(@Nullable Player player, ItemStack backpack) {
		if (player == null || !player.isCreative()) {
			return backpack.copy();
		}
		return backpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance())
				.map(IBackpackWrapper::cloneBackpack).orElse(new ItemStack(ModItems.BACKPACK.get()));
	}

	protected boolean canPlace(BlockPlaceContext context, BlockState state) {
		Player playerentity = context.getPlayer();
		CollisionContext iselectioncontext = playerentity == null ? CollisionContext.empty() : CollisionContext.of(playerentity);
		return (state.canSurvive(context.getLevel(), context.getClickedPos())) && context.getLevel().isUnobstructed(state, context.getClickedPos(), iselectioncontext);
	}

	@Override
	public InteractionResultHolder<ItemStack> use(Level world, Player player, InteractionHand hand) {
		ItemStack stack = player.getItemInHand(hand);

		if (!world.isClientSide && player instanceof ServerPlayer serverPlayer) {
			String handlerName = hand == InteractionHand.MAIN_HAND ? PlayerInventoryProvider.MAIN_INVENTORY : PlayerInventoryProvider.OFFHAND_INVENTORY;
			int slot = hand == InteractionHand.MAIN_HAND ? player.getInventory().selected : 0;
			BackpackContext.Item context = new BackpackContext.Item(handlerName, slot);
			NetworkHooks.openGui(serverPlayer, new SimpleMenuProvider((w, p, pl) -> new BackpackContainer(w, pl, context), stack.getHoverName()),
					context::toBuffer);
		}
		return InteractionResultHolder.success(stack);
	}

	@Override
	public ICapabilityProvider initCapabilities(ItemStack stack, @Nullable CompoundTag nbt) {
		return new ICapabilityProvider() {
			private IStorageWrapper wrapper = null;

			@Nonnull
			@Override
			public <T> LazyOptional<T> getCapability(Capability<T> cap, @Nullable Direction side) {
				if (stack.getCount() > 1) {
					return LazyOptional.empty();
				}

				initWrapper();
				if (cap == CapabilityBackpackWrapper.getCapabilityInstance()) {
					return LazyOptional.of(() -> wrapper).cast();
				} else if (cap == CapabilityItemHandler.ITEM_HANDLER_CAPABILITY) {
					return LazyOptional.of(() -> wrapper.getInventoryForInputOutput()).cast();
				} else if (cap == CapabilityFluidHandler.FLUID_HANDLER_ITEM_CAPABILITY && Boolean.TRUE.equals(Config.SERVER.itemFluidHandlerEnabled.get())) {
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
	public void onArmorTick(ItemStack stack, Level level, Player player) {
		if (level.isClientSide || player.isSpectator() || player.isDeadOrDying() || Boolean.FALSE.equals(Config.SERVER.nerfsConfig.onlyWornBackpackTriggersUpgrades.get())) {
			return;
		}
		stack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance()).ifPresent(
				wrapper -> wrapper.getUpgradeHandler().getWrappersThatImplement(ITickableUpgrade.class)
						.forEach(upgrade -> upgrade.tick(player, player.level, player.blockPosition()))
		);
		super.onArmorTick(stack, level, player);
	}

	@Override
	public void inventoryTick(ItemStack stack, Level level, Entity entityIn, int itemSlot, boolean isSelected) {
		if (level.isClientSide || !(entityIn instanceof Player player) || player.isSpectator() || player.isDeadOrDying() || (Config.SERVER.nerfsConfig.onlyWornBackpackTriggersUpgrades.get() && itemSlot > -1)) {
			return;
		}
		stack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance()).ifPresent(
				wrapper -> wrapper.getUpgradeHandler().getWrappersThatImplement(ITickableUpgrade.class)
						.forEach(upgrade -> upgrade.tick(player, player.level, player.blockPosition()))
		);
		super.inventoryTick(stack, level, entityIn, itemSlot, isSelected);
	}

	public int getNumberOfSlots() {
		return numberOfSlots.getAsInt();
	}

	public int getNumberOfUpgradeSlots() {
		return numberOfUpgradeSlots.getAsInt();
	}

	@Override
	public boolean onDroppedByPlayer(ItemStack item, Player player) {
		return !(player.containerMenu instanceof BackpackContainer backpackContainer && backpackContainer.getVisibleStorageItem().map(visibleStorageItem -> visibleStorageItem == item).orElse(false));
	}

	@Nullable
	@Override
	public EquipmentSlot getEquipmentSlot(ItemStack stack) {
		return EquipmentSlot.CHEST;
	}

	@Override
	public boolean shouldCauseReequipAnimation(ItemStack oldStack, ItemStack newStack, boolean slotChanged) {
		return slotChanged;
	}

	@Override
	public boolean makesPiglinsNeutral(ItemStack stack, LivingEntity wearer) {
		return stack.getItem() == ModItems.GOLD_BACKPACK.get();
	}

	@Override
	public Optional<TooltipComponent> getInventoryTooltip(ItemStack stack) {
		return Optional.of(new BackpackItem.BackpackContentsTooltip(stack));
	}

	@Override
	public ItemStack stash(ItemStack storageStack, ItemStack stack) {
		return storageStack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance()).map(wrapper -> wrapper.getInventoryForUpgradeProcessing().insertItem(stack, false)).orElse(stack);
	}

	@Override
	public StashResult getItemStashable(ItemStack storageStack, ItemStack stack) {
		return storageStack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance()).map(wrapper -> {
			if (wrapper.getInventoryForUpgradeProcessing().insertItem(stack, true).getCount() == stack.getCount()) {
				return StashResult.NO_SPACE;
			}
			if (wrapper.getInventoryHandler().getSlotTracker().getItems().contains(stack.getItem()) || wrapper.getSettingsHandler().getTypeCategory(MemorySettingsCategory.class).matchesFilter(stack)) {
				return StashResult.MATCH_AND_SPACE;
			}

			return StashResult.SPACE;
		}).orElse(StashResult.NO_SPACE);
	}

	public record BackpackContentsTooltip(ItemStack backpack) implements TooltipComponent {
		public ItemStack getBackpack() {
			return backpack;
		}
	}

	@Override
	public boolean overrideStackedOnOther(ItemStack storageStack, Slot slot, ClickAction action, Player player) {
		if (storageStack.getCount() > 1 || !slot.mayPickup(player) || action != ClickAction.SECONDARY) {
			return super.overrideStackedOnOther(storageStack, slot, action, player);
		}

		ItemStack stackToStash = slot.getItem();
		ItemStack stashResult = stash(storageStack, stackToStash);
		if (stashResult.getCount() != stackToStash.getCount()) {
			slot.set(stashResult);
			slot.onTake(player, stashResult);
			return true;
		}

		return super.overrideStackedOnOther(storageStack, slot, action, player);
	}

	@Override
	public boolean overrideOtherStackedOnMe(ItemStack storageStack, ItemStack otherStack, Slot slot, ClickAction action, Player player, SlotAccess carriedAccess) {
		if (storageStack.getCount() > 1 || !slot.mayPlace(storageStack) || action != ClickAction.SECONDARY) {
			return super.overrideOtherStackedOnMe(storageStack, otherStack, slot, action, player, carriedAccess);
		}

		ItemStack result = stash(storageStack, otherStack);
		if (result.getCount() != otherStack.getCount()) {
			carriedAccess.set(result);
			slot.set(storageStack);
			return true;
		}

		return super.overrideOtherStackedOnMe(storageStack, otherStack, slot, action, player, carriedAccess);
	}

	@Override
	public boolean canFitInsideContainerItems() {
		return Config.SERVER.canBePlacedInContainerItems.get();
	}
}