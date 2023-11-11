package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.refill;

import com.google.common.collect.ImmutableMap;
import net.minecraft.ChatFormatting;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.StringTag;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TextComponent;
import net.minecraft.network.chat.TranslatableComponent;
import net.minecraft.util.StringRepresentable;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.Level;
import net.minecraftforge.items.CapabilityItemHandler;
import net.minecraftforge.items.IItemHandler;
import net.minecraftforge.items.ItemHandlerHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBlockPickResponseUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.SBPTranslationHelper;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.inventory.ITrackedContentsItemHandler;
import net.p3pp3rf1y.sophisticatedcore.upgrades.FilterLogic;
import net.p3pp3rf1y.sophisticatedcore.upgrades.IFilteredUpgrade;
import net.p3pp3rf1y.sophisticatedcore.upgrades.ITickableUpgrade;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeWrapperBase;
import net.p3pp3rf1y.sophisticatedcore.util.InventoryHelper;
import net.p3pp3rf1y.sophisticatedcore.util.NBTHelper;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;
import java.util.function.Supplier;

public class RefillUpgradeWrapper extends UpgradeWrapperBase<RefillUpgradeWrapper, RefillUpgradeItem>
		implements IFilteredUpgrade, ITickableUpgrade, IBlockPickResponseUpgrade {
	private static final int COOLDOWN = 5;

	private final Map<Integer, TargetSlot> targetSlots;

	private final FilterLogic filterLogic;

	public RefillUpgradeWrapper(IStorageWrapper backpackWrapper, ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
		super(backpackWrapper, upgrade, upgradeSaveHandler);
		filterLogic = new FilterLogic(upgrade, upgradeSaveHandler, upgradeItem.getFilterSlotCount());
		targetSlots = NBTHelper.getMap(upgrade.getOrCreateTag(), "targetSlots", Integer::valueOf, (k, tag) -> Optional.of(TargetSlot.fromName(tag.getAsString()))).orElseGet(HashMap::new);
		if (upgradeItem.allowsTargetSlotSelection()) {
			FilterLogic.ObservableFilterItemStackHandler filterHandler = filterLogic.getFilterHandler();
			filterHandler.setOnSlotChange(s -> onFilterChange(filterHandler, s));
		}
		filterLogic.setAllowByDefault(true);
	}

	private void onFilterChange(FilterLogic.ObservableFilterItemStackHandler filterHandler, int slot) {
		if (filterHandler.getStackInSlot(slot).isEmpty()) {
			targetSlots.remove(slot);
			saveTargetSlots();
		} else {
			if (!targetSlots.containsKey(slot)) {
				setTargetSlot(slot, TargetSlot.ANY);
			}
		}
	}

	public Map<Integer, TargetSlot> getTargetSlots() {
		return targetSlots;
	}

	public void setTargetSlot(int slot, TargetSlot targetSlot) {
		targetSlots.put(slot, targetSlot);
		saveTargetSlots();
	}

	private void saveTargetSlots() {
		NBTHelper.putMap(upgrade.getOrCreateTag(), "targetSlots", targetSlots, String::valueOf, t -> StringTag.valueOf(t.getSerializedName()));
		save();
	}

	@Override
	public FilterLogic getFilterLogic() {
		return filterLogic;
	}

	@Override
	public void tick(@Nullable LivingEntity entity, Level world, BlockPos pos) {
		if (entity == null /*not supported in block form*/ || isInCooldown(world)) {
			return;
		}
		entity.getCapability(CapabilityItemHandler.ITEM_HANDLER_CAPABILITY, null).ifPresent(playerInvHandler -> InventoryHelper.iterate(filterLogic.getFilterHandler(), (slot, filter) -> {
			if (filter.isEmpty()) {
				return;
			}
			tryRefillFilter(entity, playerInvHandler, filter, getTargetSlots().getOrDefault(slot, TargetSlot.ANY));
		}));
		setCooldown(world, COOLDOWN);
	}

	private void tryRefillFilter(@Nonnull LivingEntity entity, IItemHandler playerInvHandler, ItemStack filter, TargetSlot targetSlot) {
		if (!(entity instanceof Player player)) {
			return;
		}
		int missingCount = targetSlot.missingCountGetter.getMissingCount(player, playerInvHandler, filter);
		if (ItemHandlerHelper.canItemStacksStack(player.containerMenu.getCarried(), filter)) {
			missingCount -= Math.min(missingCount, player.containerMenu.getCarried().getCount());
		}
		if (missingCount == 0) {
			return;
		}
		IItemHandler extractFromHandler = storageWrapper.getInventoryForUpgradeProcessing();
		ItemStack toMove = filter.copy();
		toMove.setCount(missingCount);
		ItemStack extracted = InventoryHelper.extractFromInventory(toMove, extractFromHandler, true);
		if (extracted.isEmpty()) {
			return;
		}
		ItemStack remaining = targetSlot.filler.fill(player, playerInvHandler, extracted);
		if (remaining.getCount() != extracted.getCount()) {
			ItemStack toExtract = extracted.copy();
			toExtract.setCount(extracted.getCount() - remaining.getCount());
			InventoryHelper.extractFromInventory(toExtract, extractFromHandler, false);
		}
	}

	public boolean allowsTargetSlotSelection() {
		return upgradeItem.allowsTargetSlotSelection();
	}

	@Override
	public boolean pickBlock(Player player, ItemStack filter) {
		if (!upgradeItem.supportsBlockPick()) {
			return false;
		}

		AtomicInteger stashSlot = new AtomicInteger(-1);
		AtomicBoolean hasItemInBackpack = new AtomicBoolean(false);

		ITrackedContentsItemHandler inventoryHandler = storageWrapper.getInventoryForUpgradeProcessing();
		InventoryHelper.iterate(inventoryHandler, (slot, stack) -> {
			if (ItemHandlerHelper.canItemStacksStack(stack, filter)) {
				hasItemInBackpack.set(true);
				stashSlot.set(slot);
			}
		}, () -> stashSlot.get() > -1);

		ItemStack mainHandItem = player.getMainHandItem();
		ItemStack toExtract = filter.copy();
		toExtract.setCount(filter.getMaxStackSize());
		if (hasItemInBackpack.get() && !InventoryHelper.extractFromInventory(toExtract, inventoryHandler, true).isEmpty()) {
			if ((inventoryHandler.getStackInSlot(stashSlot.get()).getCount() > filter.getMaxStackSize() || !inventoryHandler.isItemValid(stashSlot.get(), mainHandItem))
				&& !inventoryHandler.insertItem(mainHandItem, true).isEmpty()) {
				if (canMoveMainHandToInventory(player)) {
					ItemStack extracted = InventoryHelper.extractFromInventory(toExtract, inventoryHandler, false);
					player.setItemInHand(InteractionHand.MAIN_HAND, extracted);
					player.getInventory().add(mainHandItem);
					return true;
				} else {
					player.displayClientMessage(new TranslatableComponent("gui.sophisticatedbackpacks.status.no_space_for_mainhand_item"), true);
					return false;
				}
			} else {
				ItemStack extracted = InventoryHelper.extractFromInventory(toExtract, inventoryHandler, false);
				inventoryHandler.insertItem(mainHandItem, false);
				player.setItemInHand(InteractionHand.MAIN_HAND, extracted);
				return true;
			}
		}
		return false;
	}

	private boolean canMoveMainHandToInventory(Player player) {
		int countToAdd = player.getMainHandItem().getCount();
		for (int slot = 0; slot < player.getInventory().getContainerSize() - 5; slot++) {
			if (slot == player.getInventory().selected) {
				continue;
			}
			ItemStack slotStack = player.getInventory().getItem(slot);
			if (slotStack.isEmpty()) {
				return true;
			} else if (ItemHandlerHelper.canItemStacksStack(slotStack, player.getMainHandItem())) {
				countToAdd -= (slotStack.getMaxStackSize() - slotStack.getCount());
				if (countToAdd <= 0) {
					return true;
				}
			}
		}
		return false;
	}

	public enum TargetSlot implements StringRepresentable {
		ANY("any", SBPTranslationHelper.INSTANCE.translUpgrade("refill.target_slot.any"), SBPTranslationHelper.INSTANCE.translUpgrade("refill.target_slot.any.tooltip").withStyle(ChatFormatting.DARK_GREEN),
				(player, playerInvHandler, filter) -> InventoryHelper.getCountMissingInHandler(playerInvHandler, filter, filter.getMaxStackSize()),
				(player, playerInvHandler, stackToAdd) -> refillAnywhereInInventory(playerInvHandler, stackToAdd)),
		MAIN_HAND("main_hand", SBPTranslationHelper.INSTANCE.translUpgrade("refill.target_slot.main_hand"), SBPTranslationHelper.INSTANCE.translUpgrade("refill.target_slot.main_hand.tooltip").withStyle(ChatFormatting.DARK_GREEN),
				(player, playerInvHandler, filter) -> getMissingCount(player.getMainHandItem(), filter),
				(player, playerInvHandler, stackToAdd) -> refillSlot(player::getMainHandItem, stackToAdd, stack -> player.setItemInHand(InteractionHand.MAIN_HAND, stack))),
		OFF_HAND("off_hand", SBPTranslationHelper.INSTANCE.translUpgrade("refill.target_slot.off_hand"), SBPTranslationHelper.INSTANCE.translUpgrade("refill.target_slot.off_hand.tooltip").withStyle(ChatFormatting.DARK_GREEN),
				(player, playerInvHandler, filter) -> getMissingCount(player.getOffhandItem(), filter),
				(player, playerInvHandler, stackToAdd) -> refillSlot(player::getOffhandItem, stackToAdd, stack -> player.setItemInHand(InteractionHand.OFF_HAND, stack))),
		TOOLBAR_1("toolbar_1", new TextComponent("1"), SBPTranslationHelper.INSTANCE.translUpgrade(Constants.HOTBAR_TRANSL, 1).withStyle(ChatFormatting.DARK_GREEN),
				(player, playerInvHandler, filter) -> getMissingCount(player.getInventory().getItem(0), filter),
				(player, playerInvHandler, stackToAdd) -> refillSlot(() -> player.getInventory().getItem(0), stackToAdd, stack -> player.getInventory().setItem(0, stack))),
		TOOLBAR_2("toolbar_2", new TextComponent("2"), SBPTranslationHelper.INSTANCE.translUpgrade(Constants.HOTBAR_TRANSL, 2).withStyle(ChatFormatting.DARK_GREEN),
				(player, playerInvHandler, filter) -> getMissingCount(player.getInventory().getItem(1), filter),
				(player, playerInvHandler, stackToAdd) -> refillSlot(() -> player.getInventory().getItem(1), stackToAdd, stack -> player.getInventory().setItem(1, stack))),
		TOOLBAR_3("toolbar_3", new TextComponent("3"), SBPTranslationHelper.INSTANCE.translUpgrade(Constants.HOTBAR_TRANSL, 3).withStyle(ChatFormatting.DARK_GREEN),
				(player, playerInvHandler, filter) -> getMissingCount(player.getInventory().getItem(2), filter),
				(player, playerInvHandler, stackToAdd) -> refillSlot(() -> player.getInventory().getItem(2), stackToAdd, stack -> player.getInventory().setItem(2, stack))),
		TOOLBAR_4("toolbar_4", new TextComponent("4"), SBPTranslationHelper.INSTANCE.translUpgrade(Constants.HOTBAR_TRANSL, 4).withStyle(ChatFormatting.DARK_GREEN),
				(player, playerInvHandler, filter) -> getMissingCount(player.getInventory().getItem(3), filter),
				(player, playerInvHandler, stackToAdd) -> refillSlot(() -> player.getInventory().getItem(3), stackToAdd, stack -> player.getInventory().setItem(3, stack))),
		TOOLBAR_5("toolbar_5", new TextComponent("5"), SBPTranslationHelper.INSTANCE.translUpgrade(Constants.HOTBAR_TRANSL, 5).withStyle(ChatFormatting.DARK_GREEN),
				(player, playerInvHandler, filter) -> getMissingCount(player.getInventory().getItem(4), filter),
				(player, playerInvHandler, stackToAdd) -> refillSlot(() -> player.getInventory().getItem(4), stackToAdd, stack -> player.getInventory().setItem(4, stack))),
		TOOLBAR_6("toolbar_6", new TextComponent("6"), SBPTranslationHelper.INSTANCE.translUpgrade(Constants.HOTBAR_TRANSL, 6).withStyle(ChatFormatting.DARK_GREEN),
				(player, playerInvHandler, filter) -> getMissingCount(player.getInventory().getItem(5), filter),
				(player, playerInvHandler, stackToAdd) -> refillSlot(() -> player.getInventory().getItem(5), stackToAdd, stack -> player.getInventory().setItem(5, stack))),
		TOOLBAR_7("toolbar_7", new TextComponent("7"), SBPTranslationHelper.INSTANCE.translUpgrade(Constants.HOTBAR_TRANSL, 7).withStyle(ChatFormatting.DARK_GREEN),
				(player, playerInvHandler, filter) -> getMissingCount(player.getInventory().getItem(6), filter),
				(player, playerInvHandler, stackToAdd) -> refillSlot(() -> player.getInventory().getItem(6), stackToAdd, stack -> player.getInventory().setItem(6, stack))),
		TOOLBAR_8("toolbar_8", new TextComponent("8"), SBPTranslationHelper.INSTANCE.translUpgrade(Constants.HOTBAR_TRANSL, 8).withStyle(ChatFormatting.DARK_GREEN),
				(player, playerInvHandler, filter) -> getMissingCount(player.getInventory().getItem(7), filter),
				(player, playerInvHandler, stackToAdd) -> refillSlot(() -> player.getInventory().getItem(7), stackToAdd, stack -> player.getInventory().setItem(7, stack))),
		TOOLBAR_9("toolbar_9", new TextComponent("9"), SBPTranslationHelper.INSTANCE.translUpgrade(Constants.HOTBAR_TRANSL, 9).withStyle(ChatFormatting.DARK_GREEN),
				(player, playerInvHandler, filter) -> getMissingCount(player.getInventory().getItem(8), filter),
				(player, playerInvHandler, stackToAdd) -> refillSlot(() -> player.getInventory().getItem(8), stackToAdd, stack -> player.getInventory().setItem(8, stack)));

		private final String name;

		private final Component acronym;
		private final Component description;
		private final MissingCountGetter missingCountGetter;
		private final Filler filler;

		TargetSlot(String name, Component acronym, Component description, MissingCountGetter missingCountGetter, Filler filler) {
			this.name = name;
			this.acronym = acronym;
			this.description = description;
			this.missingCountGetter = missingCountGetter;
			this.filler = filler;
		}

		@Override
		public String getSerializedName() {
			return name;
		}

		public TargetSlot next() {
			return VALUES[(ordinal() + 1) % VALUES.length];
		}

		public TargetSlot previous() {
			return VALUES[Math.floorMod(ordinal() - 1, VALUES.length)];
		}

		private static final Map<String, TargetSlot> NAME_VALUES;
		private static final TargetSlot[] VALUES;

		static {
			ImmutableMap.Builder<String, TargetSlot> builder = new ImmutableMap.Builder<>();
			for (TargetSlot value : TargetSlot.values()) {
				builder.put(value.getSerializedName(), value);
			}
			NAME_VALUES = builder.build();
			VALUES = values();
		}

		public static TargetSlot fromName(String name) {
			return NAME_VALUES.getOrDefault(name, ANY);
		}

		public Component getAcronym() {
			return acronym;
		}

		public Component getDescription() {
			return description;
		}

		private static class Constants {
			private static final String HOTBAR_TRANSL = "refill.target_slot.hotbar.tooltip";
		}

		private interface MissingCountGetter {
			int getMissingCount(Player player, IItemHandler playerInventory, ItemStack filter);
		}

		private interface Filler {
			ItemStack fill(Player player, IItemHandler playerInventory, ItemStack stackToAdd);
		}

		private static ItemStack refillAnywhereInInventory(IItemHandler playerInvHandler, ItemStack extracted) {
			AtomicReference<ItemStack> remainingStack = new AtomicReference<>(extracted);
			InventoryHelper.iterate(playerInvHandler, (slot, stack) -> {
				if (ItemHandlerHelper.canItemStacksStack(stack, remainingStack.get())) {
					remainingStack.set(playerInvHandler.insertItem(slot, remainingStack.get(), false));
				}
			}, () -> remainingStack.get().isEmpty());

			ItemStack remaining = remainingStack.get();

			if (!remaining.isEmpty()) {
				ItemStack afterInsert = InventoryHelper.insertIntoInventory(remaining, playerInvHandler, true);
				if (afterInsert.getCount() == remaining.getCount()) {
					return remaining;
				}
				ItemStack toInsert = remaining.copy();
				toInsert.setCount(remaining.getCount() - afterInsert.getCount());
				return InventoryHelper.insertIntoInventory(toInsert, playerInvHandler, false);
			}
			return remaining;
		}

		private static int getMissingCount(ItemStack stack, ItemStack filter) {
			if (ItemHandlerHelper.canItemStacksStack(stack, filter)) {
				return filter.getMaxStackSize() - stack.getCount();
			}
			return filter.getMaxStackSize();
		}

		private static ItemStack refillSlot(Supplier<ItemStack> getSlotContents, ItemStack stackToAdd, Consumer<ItemStack> setSlotContents) {
			ItemStack contents = getSlotContents.get();
			if (contents.isEmpty()) {
				setSlotContents.accept(stackToAdd);
				return ItemStack.EMPTY;
			}
			if (ItemHandlerHelper.canItemStacksStack(contents, stackToAdd)) {
				contents.grow(stackToAdd.getCount());
				return ItemStack.EMPTY;
			}
			return stackToAdd;
		}
	}
}

