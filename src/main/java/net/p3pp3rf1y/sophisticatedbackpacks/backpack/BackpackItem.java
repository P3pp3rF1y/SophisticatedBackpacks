package net.p3pp3rf1y.sophisticatedbackpacks.backpack;

import net.minecraft.ChatFormatting;
import net.minecraft.client.gui.screens.Screen;
import net.minecraft.client.gui.screens.inventory.CreativeModeInventoryScreen;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.core.HolderLookup;
import net.minecraft.network.chat.Component;
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
import net.minecraft.world.item.DyeColor;
import net.minecraft.world.item.Item;
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
import net.neoforged.fml.loading.FMLEnvironment;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.LegacyBackpackDataMigration;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContext;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.everlasting.EverlastingBackpackItemEntity;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.everlasting.EverlastingUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.util.InventoryInteractionHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.PlayerInventoryProvider;
import net.p3pp3rf1y.sophisticatedcore.api.IStashStorageItem;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.TranslationHelper;
import net.p3pp3rf1y.sophisticatedcore.init.ModCoreDataComponents;
import net.p3pp3rf1y.sophisticatedcore.settings.memory.MemorySettingsCategory;
import net.p3pp3rf1y.sophisticatedcore.upgrades.ITickableUpgrade;
import net.p3pp3rf1y.sophisticatedcore.upgrades.jukebox.ServerStorageSoundHandler;
import net.p3pp3rf1y.sophisticatedcore.util.ColorHelper;
import net.p3pp3rf1y.sophisticatedcore.util.ItemBase;
import net.p3pp3rf1y.sophisticatedcore.util.WorldHelper;

import javax.annotation.Nullable;
import java.util.List;
import java.util.Optional;
import java.util.function.Consumer;
import java.util.function.IntSupplier;
import java.util.function.Supplier;
import java.util.function.UnaryOperator;

import static net.minecraft.world.level.block.state.properties.BlockStateProperties.WATERLOGGED;

public class BackpackItem extends ItemBase implements IStashStorageItem {
	public static final int DEFAULT_MAIN_COLOR = BackpackWrapper.DEFAULT_MAIN_COLOR;
	public static final int DEFAULT_ACCENT_COLOR = BackpackWrapper.DEFAULT_ACCENT_COLOR;

	private final IntSupplier numberOfSlots;
	private final IntSupplier numberOfUpgradeSlots;
	private final Supplier<BackpackBlock> blockSupplier;

	public BackpackItem(IntSupplier numberOfSlots, IntSupplier numberOfUpgradeSlots, Supplier<BackpackBlock> blockSupplier) {
		this(numberOfSlots, numberOfUpgradeSlots, blockSupplier, p -> p);
	}

	public BackpackItem(IntSupplier numberOfSlots, IntSupplier numberOfUpgradeSlots, Supplier<BackpackBlock> blockSupplier, UnaryOperator<Properties> updateProperties) {
		super(updateProperties.apply(new Properties().stacksTo(1)));
		this.numberOfSlots = numberOfSlots;
		this.numberOfUpgradeSlots = numberOfUpgradeSlots;
		this.blockSupplier = blockSupplier;
	}

	public static void setColors(ItemStack backpackStack, int mainColor, int accentColor) {
		backpackStack.set(ModCoreDataComponents.MAIN_COLOR, LegacyBackpackDataMigration.withOpaqueAlpha(mainColor));
		backpackStack.set(ModCoreDataComponents.ACCENT_COLOR, LegacyBackpackDataMigration.withOpaqueAlpha(accentColor));
	}

	public static int getMainColor(ItemStack backpackStack) {
		Integer mainColor = backpackStack.get(ModCoreDataComponents.MAIN_COLOR);
		if (mainColor != null) {
			int normalizedMainColor = LegacyBackpackDataMigration.withOpaqueAlpha(mainColor);
			if (normalizedMainColor != mainColor) {
				backpackStack.set(ModCoreDataComponents.MAIN_COLOR, normalizedMainColor);
			}
			return normalizedMainColor;
		}

		return LegacyBackpackDataMigration.getMainColor(backpackStack).map(legacyMainColor -> {
			backpackStack.set(ModCoreDataComponents.MAIN_COLOR, legacyMainColor);
			return legacyMainColor;
		}).orElse(DEFAULT_MAIN_COLOR);
	}

	public static int getAccentColor(ItemStack backpackStack) {
		Integer accentColor = backpackStack.get(ModCoreDataComponents.ACCENT_COLOR);
		if (accentColor != null) {
			int normalizedAccentColor = LegacyBackpackDataMigration.withOpaqueAlpha(accentColor);
			if (normalizedAccentColor != accentColor) {
				backpackStack.set(ModCoreDataComponents.ACCENT_COLOR, normalizedAccentColor);
			}
			return normalizedAccentColor;
		}

		return LegacyBackpackDataMigration.getAccentColor(backpackStack).map(legacyAccentColor -> {
			backpackStack.set(ModCoreDataComponents.ACCENT_COLOR, legacyAccentColor);
			return legacyAccentColor;
		}).orElse(DEFAULT_ACCENT_COLOR);
	}

	@Override
	public void addCreativeTabItems(Consumer<ItemStack> itemConsumer) {
		super.addCreativeTabItems(itemConsumer);

		if (this != ModItems.BACKPACK.get() || !net.p3pp3rf1y.sophisticatedcore.Config.COMMON.enabledItems.isItemEnabled(this)) {
			return;
		}

		for (DyeColor color : DyeColor.values()) {
			ItemStack stack = new ItemStack(this);
			setColors(stack, color.getTextureDiffuseColor(), color.getTextureDiffuseColor());
			itemConsumer.accept(stack);
		}

		int mainColor = ColorHelper.calculateColor(BackpackWrapper.DEFAULT_MAIN_COLOR, BackpackWrapper.DEFAULT_MAIN_COLOR, List.of(
				DyeColor.YELLOW, DyeColor.LIME
		));
		int accentColor = ColorHelper.calculateColor(BackpackWrapper.DEFAULT_ACCENT_COLOR, BackpackWrapper.DEFAULT_ACCENT_COLOR, List.of(
				DyeColor.BLUE, DyeColor.BLACK
		));

		ItemStack stack = new ItemStack(this);
		setColors(stack, mainColor, accentColor);
		itemConsumer.accept(stack);
	}

	@Override
	public void appendHoverText(ItemStack stack, Item.TooltipContext context, List<Component> tooltip, TooltipFlag tooltipFlag) {
		super.appendHoverText(stack, context, tooltip, tooltipFlag);
		if (tooltipFlag.isAdvanced()) {
			BackpackWrapper.fromStack(stack).getContentsUuid()
					.ifPresent(uuid -> tooltip.add(Component.literal("UUID: " + uuid).withStyle(ChatFormatting.DARK_GRAY)));
		}
		if (!Screen.hasShiftDown()) {
			tooltip.add(Component.translatable(
					TranslationHelper.INSTANCE.translItemTooltip("storage") + ".press_for_contents",
					Component.translatable(TranslationHelper.INSTANCE.translItemTooltip("storage") + ".shift").withStyle(ChatFormatting.AQUA)
			).withStyle(ChatFormatting.GRAY));
		}
	}

	@Override
	public Optional<TooltipComponent> getTooltipImage(ItemStack stack) {
		if (FMLEnvironment.dist.isClient()) {
			return Optional.ofNullable(BackpackItemClient.getTooltipImage(stack));
		}
		return Optional.empty();
	}

	@Override
	public boolean hasCustomEntity(ItemStack stack) {
		return true;
	}

	private boolean hasEverlastingUpgrade(ItemStack stack) {
		return !BackpackWrapper.fromStack(stack).getUpgradeHandler().getTypeWrappers(EverlastingUpgradeItem.TYPE).isEmpty();
	}

	@Nullable
	@Override
	public Entity createEntity(Level level, Entity entity, ItemStack itemstack) {
		if (!(entity instanceof ItemEntity itemEntity)) {
			return null;
		}

		UUIDDeduplicator.dedupeBackpackItemEntityInArea(itemEntity);

		return hasEverlastingUpgrade(itemstack) ? createEverlastingBackpack(level, (ItemEntity) entity, itemstack) : null;
	}

	@Nullable
	private EverlastingBackpackItemEntity createEverlastingBackpack(Level level, ItemEntity itemEntity, ItemStack itemstack) {
		EverlastingBackpackItemEntity backpackItemEntity = ModItems.EVERLASTING_BACKPACK_ITEM_ENTITY.get().create(level);
		if (backpackItemEntity != null) {
			backpackItemEntity.setPos(itemEntity.getX(), itemEntity.getY(), itemEntity.getZ());
			backpackItemEntity.setItem(itemstack);
			backpackItemEntity.setPickUpDelay(itemEntity.pickupDelay);
			if (itemEntity.getOwner() != null) {
				backpackItemEntity.setThrower(itemEntity.getOwner());
			}
			backpackItemEntity.setDeltaMovement(itemEntity.getDeltaMovement());
		}
		return backpackItemEntity;
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
		Level level = blockItemUseContext.getLevel();
		BlockPos pos = blockItemUseContext.getClickedPos();

		FluidState fluidstate = blockItemUseContext.getLevel().getFluidState(pos);
		BlockState placementState = blockSupplier.get().defaultBlockState().setValue(BackpackBlock.FACING, direction)
				.setValue(WATERLOGGED, fluidstate.getType() == Fluids.WATER);
		if (!canPlace(blockItemUseContext, placementState)) {
			return InteractionResult.FAIL;
		}

		if (level.setBlockAndUpdate(pos, placementState)) {
			ItemStack backpack = blockItemUseContext.getItemInHand();
			WorldHelper.getBlockEntity(level, pos, BackpackBlockEntity.class).ifPresent(be -> {
				be.setBackpack(getBackpackCopy(player, backpack));
				be.refreshRenderState();

				be.tryToAddToController();
			});

			if (!level.isClientSide) {
				stopBackpackSounds(backpack, level, pos);
			}

			SoundType soundtype = placementState.getSoundType(level, pos, player);
			level.playSound(player, pos, soundtype.getPlaceSound(), SoundSource.BLOCKS, (soundtype.getVolume() + 1.0F) / 2.0F, soundtype.getPitch() * 0.8F);
			if (player == null || !player.isCreative()) {
				backpack.shrink(1);
			}

			return InteractionResult.SUCCESS;
		}
		return InteractionResult.PASS;
	}

	private static void stopBackpackSounds(ItemStack backpack, Level level, BlockPos pos) {
		BackpackWrapper.fromStack(backpack)
				.getContentsUuid()
				.ifPresent(uuid -> ServerStorageSoundHandler.stopPlayingDisc(level, Vec3.atCenterOf(pos), uuid));
	}

	private ItemStack getBackpackCopy(@Nullable Player player, ItemStack backpack) {
		if (player == null || !player.isCreative()) {
			return backpack.copy();
		}
		return BackpackWrapper.fromStack(backpack).cloneBackpack();
	}

	protected boolean canPlace(BlockPlaceContext context, BlockState state) {
		Player playerentity = context.getPlayer();
		CollisionContext iselectioncontext = playerentity == null ? CollisionContext.empty() : CollisionContext.of(playerentity);
		return (state.canSurvive(context.getLevel(), context.getClickedPos())) && context.getLevel().isUnobstructed(state, context.getClickedPos(), iselectioncontext);
	}

	@Override
	public InteractionResultHolder<ItemStack> use(Level level, Player player, InteractionHand hand) {
		ItemStack stack = player.getItemInHand(hand);

		if (!level.isClientSide) {
			String handlerName = hand == InteractionHand.MAIN_HAND ? PlayerInventoryProvider.MAIN_INVENTORY : PlayerInventoryProvider.OFFHAND_INVENTORY;
			int slot = hand == InteractionHand.MAIN_HAND ? player.getInventory().selected : 0;
			BackpackContext.Item context = new BackpackContext.Item(handlerName, slot);
			player.openMenu(new SimpleMenuProvider((w, p, pl) -> new BackpackContainer(w, pl, context), stack.getHoverName()), context::toBuffer);
		}
		return InteractionResultHolder.success(stack);
	}

	@Override
	public void inventoryTick(ItemStack stack, Level level, Entity entity, int itemSlot, boolean isSelected) {
		if (!(entity instanceof Player player) || player.isSpectator() || player.isDeadOrDying() || (Config.SERVER.nerfsConfig.onlyWornBackpackTriggersUpgrades.get() && itemSlot > -1)) {
			return;
		}
		IBackpackWrapper backpackWrapper = BackpackWrapper.fromStack(stack);
		backpackWrapper.getUpgradeHandler().getWrappersThatImplement(ITickableUpgrade.class)
				.forEach(upgrade -> {
					if (level.isClientSide) {
						upgrade.clientTick(player, player.level(), player.blockPosition());
					} else {
						upgrade.tick(player, player.level(), player.blockPosition());
					}
				}
				);
		super.inventoryTick(stack, level, entity, itemSlot, isSelected);
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
		return Optional.of(new BackpackContentsTooltip(stack));
	}

	public ItemStack stash(ItemStack storageStack, ItemStack stack, boolean simulate) {
		return BackpackWrapper.fromStack(storageStack).getInventoryForUpgradeProcessing().insertItem(stack, simulate);
	}

	@Override
	public StashResult getItemStashable(HolderLookup.Provider registries, ItemStack storageStack, ItemStack stack) {
		IBackpackWrapper wrapper = BackpackWrapper.fromStack(storageStack);
		if (wrapper.getContentsUuid().isEmpty()) {
			return StashResult.SPACE; //Assuming that backpack that has no contentsUuid is empty and will have inventory once contentsUuid is created and thus any item can be stashed into it
		}

		if (wrapper.getInventoryForUpgradeProcessing().insertItem(stack, true).getCount() == stack.getCount()) {
			return StashResult.NO_SPACE;
		}
		if (wrapper.getInventoryHandler().getSlotTracker().getItems().contains(stack.getItem()) || wrapper.getSettingsHandler().getTypeCategory(MemorySettingsCategory.class).matchesFilter(stack)) {
			return StashResult.MATCH_AND_SPACE;
		}

		return StashResult.SPACE;
	}

	public record BackpackContentsTooltip(ItemStack backpack) implements TooltipComponent {
		public ItemStack getBackpack() {
			return backpack;
		}
	}

	@Override
	public boolean overrideStackedOnOther(ItemStack storageStack, Slot slot, ClickAction action, Player player) {
		if (hasCreativeScreenContainerOpen(player) || storageStack.getCount() > 1 || !slot.mayPickup(player) || action != ClickAction.SECONDARY) {
			return super.overrideStackedOnOther(storageStack, slot, action, player);
		}

		ItemStack stackToStash = slot.getItem();
		ItemStack stashResult = stash(storageStack, stackToStash, true);
		if (stashResult.getCount() < stackToStash.getCount()) {
			int countToTake = stackToStash.getCount() - stashResult.getCount();
			while (countToTake > 0) {
				ItemStack takeResult = slot.safeTake(countToTake, countToTake, player);
				if (takeResult.isEmpty()) {
					break;
				}
				stash(storageStack, takeResult, false);
				countToTake -= takeResult.getCount();
			}
			return true;
		}

		return super.overrideStackedOnOther(storageStack, slot, action, player);
	}

	@Override
	public boolean overrideOtherStackedOnMe(ItemStack storageStack, ItemStack otherStack, Slot slot, ClickAction action, Player player, SlotAccess carriedAccess) {
		if (hasCreativeScreenContainerOpen(player) || storageStack.getCount() > 1 || !slot.mayPlace(storageStack) || action != ClickAction.SECONDARY) {
			return super.overrideOtherStackedOnMe(storageStack, otherStack, slot, action, player, carriedAccess);
		}

		ItemStack result = stash(storageStack, otherStack, false);
		if (result.getCount() != otherStack.getCount()) {
			carriedAccess.set(result);
			slot.set(storageStack);
			return true;
		}

		return super.overrideOtherStackedOnMe(storageStack, otherStack, slot, action, player, carriedAccess);
	}

	private boolean hasCreativeScreenContainerOpen(Player player) {
		return player.level().isClientSide() && player.containerMenu instanceof CreativeModeInventoryScreen.ItemPickerMenu;
	}

	@Override
	public boolean canFitInsideContainerItems() {
		return Config.SERVER.canBePlacedInContainerItems.get();
	}
}
