package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.refill;

import com.google.common.collect.ImmutableMap;
import com.mojang.serialization.Codec;
import net.minecraft.ChatFormatting;
import net.minecraft.core.BlockPos;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.chat.Component;
import net.minecraft.network.codec.ByteBufCodecs;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.util.StringRepresentable;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.Level;
import net.minecraft.world.phys.AABB;
import net.neoforged.neoforge.capabilities.Capabilities;
import net.neoforged.neoforge.network.codec.NeoForgeStreamCodecs;
import net.neoforged.neoforge.transfer.ResourceHandler;
import net.neoforged.neoforge.transfer.item.ItemResource;
import net.neoforged.neoforge.transfer.transaction.Transaction;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBlockPickResponseUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackTranslationHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModDataComponents;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.init.ModCoreDataComponents;
import net.p3pp3rf1y.sophisticatedcore.upgrades.FilterLogic;
import net.p3pp3rf1y.sophisticatedcore.upgrades.IFilteredUpgrade;
import net.p3pp3rf1y.sophisticatedcore.upgrades.ITickableUpgrade;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeWrapperBase;
import net.p3pp3rf1y.sophisticatedcore.util.CapabilityHelper;
import net.p3pp3rf1y.sophisticatedcore.util.CodecHelper;
import net.p3pp3rf1y.sophisticatedcore.util.InventoryHelper;
import net.p3pp3rf1y.sophisticatedcore.util.StreamCodecHelper;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;
import java.util.function.Supplier;

public class RefillUpgradeWrapper extends UpgradeWrapperBase<RefillUpgradeWrapper, RefillUpgradeItem>
		implements
			IFilteredUpgrade,
			ITickableUpgrade,
			IBlockPickResponseUpgrade {
	private static final int REFILL_RANGE = 3;
	private static final int COOLDOWN = 5;

	public static final Codec<Map<Integer, TargetSlot>> TARGET_SLOTS_CODEC = Codec.unboundedMap(CodecHelper.STRING_ENCODED_INT, TargetSlot.CODEC);
	public static final StreamCodec<FriendlyByteBuf, Map<Integer, TargetSlot>> TARGET_SLOTS_STREAM_CODEC = StreamCodecHelper.ofMap(ByteBufCodecs.INT,
			TargetSlot.STREAM_CODEC, HashMap::new);

	private final Map<Integer, TargetSlot> targetSlots;

	private final FilterLogic filterLogic;

	public RefillUpgradeWrapper(IStorageWrapper backpackWrapper, ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
		super(backpackWrapper, upgrade, upgradeSaveHandler);
		filterLogic = new FilterLogic(upgrade, upgradeSaveHandler, upgradeItem.getFilterSlotCount(), ModCoreDataComponents.FILTER_ATTRIBUTES);
		targetSlots = new HashMap<>(upgrade.getOrDefault(ModDataComponents.TARGET_SLOTS, new HashMap<>()));
		if (upgradeItem.allowsTargetSlotSelection()) {
			FilterLogic.ObservableFilterItemStackHandler filterHandler = filterLogic.getFilterHandler();
			filterHandler.setOnSlotChange(s -> onFilterChange(filterHandler, s));
		}
		filterLogic.setAllowByDefault(true);
	}

	private void onFilterChange(FilterLogic.ObservableFilterItemStackHandler filterHandler, int slot) {
		if (filterHandler.getResource(slot).isEmpty()) {
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
		upgrade.set(ModDataComponents.TARGET_SLOTS, ImmutableMap.copyOf(targetSlots));
		save();
	}

	@Override
	public FilterLogic getFilterLogic() {
		return filterLogic;
	}

	@Override
	public void tick(@Nullable Entity entity, Level level, BlockPos pos) {
		if (isInCooldown(level)) {
			return;
		}

		if (!(entity instanceof Player)) {
			level.getEntities(EntityType.PLAYER, new AABB(pos).inflate(REFILL_RANGE), p -> true).forEach(this::refillItemFor);
		} else {
			refillItemFor(entity);
		}
		setCooldown(level, COOLDOWN);
	}

	private void refillItemFor(Entity entity) {
		CapabilityHelper.runOnItemHandler(entity,
				playerInvHandler -> InventoryHelper.iterate(filterLogic.getFilterHandler(), (slot, filterResource, amount) -> {
					if (filterResource.isEmpty()) {
						return;
					}
					tryRefillFilter(entity, playerInvHandler, filterResource, getTargetSlots().getOrDefault(slot, TargetSlot.ANY));
				}));
	}

	private void tryRefillFilter(@Nonnull Entity entity, ResourceHandler<ItemResource> playerInvHandler, ItemResource filter, TargetSlot targetSlot) {
		if (!(entity instanceof Player player)) {
			return;
		}
		int missingCount = targetSlot.missingCountGetter.getMissingCount(player, playerInvHandler, filter);
		ItemStack stack = player.containerMenu.getCarried();
		if (filter.matches(stack)) {
			missingCount -= Math.min(missingCount, player.containerMenu.getCarried().getCount());
		}
		if (missingCount == 0) {
			return;
		}
		ResourceHandler<ItemResource> extractFromHandler = storageWrapper.getInventoryForUpgradeProcessing();
		int extracted = InventoryHelper.simulateExtractExact(extractFromHandler, filter, missingCount);
		if (extracted == 0) {
			return;
		}
		int filled = targetSlot.filler.fill(player, playerInvHandler, filter, extracted);
		if (filled > 0) {
			InventoryHelper.extract(extractFromHandler, filter, filled);
		}
	}

	public boolean allowsTargetSlotSelection() {
		return upgradeItem.allowsTargetSlotSelection();
	}

	@Override
	public boolean pickBlock(Player player, ItemStack filter) {
		if (!upgradeItem.supportsBlockPick() || filter.isEmpty()) {
			return false;
		}

		var handler = storageWrapper.getInventoryForUpgradeProcessing(); // ResourceHandler<ItemResource>

		try (Transaction tx = Transaction.openRoot()) {
			int pulled = handler.extract(ItemResource.of(filter), filter.getMaxStackSize(), tx);
			if (pulled <= 0) {
				return false;
			}

			int slotToUse = player.getInventory().getSuitableHotbarSlot();
			ItemStack stackInSlot = player.getInventory().getItem(slotToUse);

			boolean canStashHand = !(stackInSlot.getItem() instanceof BackpackItem)
					&& (stackInSlot.isEmpty() || handler.insert(ItemResource.of(stackInSlot), stackInSlot.getCount(), tx) == stackInSlot.getCount());

			if (canStashHand) {
				tx.commit();
				player.getInventory().setSelectedSlot(slotToUse);
				player.getInventory().setSelectedItem(filter.copyWithCount(pulled));
				return true;
			} else if (canMoveMainHandToInventory(player)) {
				tx.commit();
				if (!stackInSlot.isEmpty()) {
					player.getInventory().add(stackInSlot.copy());
				}
				player.getInventory().setSelectedSlot(slotToUse);
				player.getInventory().setSelectedItem(filter.copyWithCount(pulled));
				return true;
			} else {
				player.displayClientMessage(Component.translatable("gui.sophisticatedbackpacks.status.no_space_for_mainhand_item"), true);
				return false;
			}
		}
	}

	private boolean canMoveMainHandToInventory(Player player) {
		ResourceHandler<ItemResource> capability = player.getCapability(Capabilities.Item.ENTITY);
		if (capability == null) {
			return false;
		}
		AtomicInteger countAdded = new AtomicInteger();
		return InventoryHelper.iterate(capability, (slot, resource, amount) -> {
			if (slot > 35 || slot == player.getInventory().getSelectedSlot()) {
				return false;
			}
			if (resource.isEmpty()) {
				return true;
			}
			if (resource.equals(ItemResource.of(player.getMainHandItem()))) {
				countAdded.addAndGet(Math.min(player.getMainHandItem().getCount() - countAdded.get(), resource.getMaxStackSize() - amount));
				if (countAdded.get() >= player.getMainHandItem().getCount()) {
					return true;
				}
			}
			return false;
		}, () -> false, returnValue -> returnValue);
	}

	public enum TargetSlot implements StringRepresentable {
		ANY("any", BackpackTranslationHelper.INSTANCE.translUpgrade("refill.target_slot.any"),
				BackpackTranslationHelper.INSTANCE.translUpgrade("refill.target_slot.any.tooltip").withStyle(ChatFormatting.DARK_GREEN),
				(player, playerInvHandler, filter) -> InventoryHelper.getCountMissingInHandler(playerInvHandler, filter, filter.getMaxStackSize()),
				(player, playerInvHandler, resourceToAdd, amountToAdd) -> refillAnywhereInInventory(playerInvHandler, resourceToAdd, amountToAdd)), MAIN_HAND(
						"main_hand", BackpackTranslationHelper.INSTANCE.translUpgrade("refill.target_slot.main_hand"),
						BackpackTranslationHelper.INSTANCE.translUpgrade("refill.target_slot.main_hand.tooltip").withStyle(ChatFormatting.DARK_GREEN),
						(player, playerInvHandler, filter) -> getMissingCount(player.getMainHandItem(), filter),
						(player, playerInvHandler, resourceToAdd, amountToAdd) -> refillSlot(player::getMainHandItem, resourceToAdd, amountToAdd,
								stack -> player.setItemInHand(InteractionHand.MAIN_HAND, stack))), OFF_HAND("off_hand",
										BackpackTranslationHelper.INSTANCE.translUpgrade("refill.target_slot.off_hand"),
										BackpackTranslationHelper.INSTANCE.translUpgrade("refill.target_slot.off_hand.tooltip")
												.withStyle(ChatFormatting.DARK_GREEN),
										(player, playerInvHandler, filter) -> getMissingCount(player.getOffhandItem(), filter),
										(player, playerInvHandler, resourceToAdd, amountToAdd) -> refillSlot(player::getOffhandItem, resourceToAdd, amountToAdd,
												stack -> player.setItemInHand(InteractionHand.OFF_HAND, stack))), TOOLBAR_1(
														"toolbar_1", Component.literal("1"),
														BackpackTranslationHelper.INSTANCE.translUpgrade(Constants.HOTBAR_TRANSL, 1)
																.withStyle(ChatFormatting.DARK_GREEN),
														(player, playerInvHandler, filter) -> getMissingCount(player.getInventory().getItem(0), filter),
														(player, playerInvHandler, resourceToAdd, amountToAdd) -> refillSlot(
																() -> player.getInventory().getItem(0), resourceToAdd, amountToAdd,
																stack -> player.getInventory().setItem(0, stack))), TOOLBAR_2("toolbar_2",
																		Component.literal("2"),
																		BackpackTranslationHelper.INSTANCE.translUpgrade(Constants.HOTBAR_TRANSL, 2)
																				.withStyle(ChatFormatting.DARK_GREEN),
																		(player, playerInvHandler, filter) -> getMissingCount(player.getInventory().getItem(1),
																				filter),
																		(player, playerInvHandler, resourceToAdd, amountToAdd) -> refillSlot(
																				() -> player.getInventory().getItem(1), resourceToAdd, amountToAdd,
																				stack -> player.getInventory().setItem(1, stack))), TOOLBAR_3("toolbar_3",
																						Component.literal("3"),
																						BackpackTranslationHelper.INSTANCE
																								.translUpgrade(Constants.HOTBAR_TRANSL, 3)
																								.withStyle(ChatFormatting.DARK_GREEN),
																						(player, playerInvHandler, filter) -> getMissingCount(
																								player.getInventory().getItem(2), filter),
																						(player, playerInvHandler, resourceToAdd, amountToAdd) -> refillSlot(
																								() -> player.getInventory().getItem(2), resourceToAdd,
																								amountToAdd,
																								stack -> player.getInventory().setItem(2, stack))), TOOLBAR_4(
																										"toolbar_4", Component.literal("4"),
																										BackpackTranslationHelper.INSTANCE
																												.translUpgrade(Constants.HOTBAR_TRANSL, 4)
																												.withStyle(ChatFormatting.DARK_GREEN),
																										(player, playerInvHandler, filter) -> getMissingCount(
																												player.getInventory().getItem(3), filter),
																										(player, playerInvHandler, resourceToAdd,
																												amountToAdd) -> refillSlot(
																														() -> player.getInventory().getItem(3),
																														resourceToAdd, amountToAdd,
																														stack -> player.getInventory()
																																.setItem(3, stack))), TOOLBAR_5(
																																		"toolbar_5",
																																		Component.literal("5"),
																																		BackpackTranslationHelper.INSTANCE
																																				.translUpgrade(
																																						Constants.HOTBAR_TRANSL,
																																						5)
																																				.withStyle(
																																						ChatFormatting.DARK_GREEN),
																																		(player, playerInvHandler,
																																				filter) -> getMissingCount(
																																						player.getInventory()
																																								.getItem(
																																										4),
																																						filter),
																																		(player, playerInvHandler,
																																				resourceToAdd,
																																				amountToAdd) -> refillSlot(
																																						() -> player
																																								.getInventory()
																																								.getItem(
																																										4),
																																						resourceToAdd,
																																						amountToAdd,
																																						stack -> player
																																								.getInventory()
																																								.setItem(
																																										4,
																																										stack))), TOOLBAR_6(
																																												"toolbar_6",
																																												Component
																																														.literal(
																																																"6"),
																																												BackpackTranslationHelper.INSTANCE
																																														.translUpgrade(
																																																Constants.HOTBAR_TRANSL,
																																																6)
																																														.withStyle(
																																																ChatFormatting.DARK_GREEN),
																																												(player, playerInvHandler,
																																														filter) -> getMissingCount(
																																																player.getInventory()
																																																		.getItem(
																																																				5),
																																																filter),
																																												(player, playerInvHandler,
																																														resourceToAdd,
																																														amountToAdd) -> refillSlot(
																																																() -> player
																																																		.getInventory()
																																																		.getItem(
																																																				5),
																																																resourceToAdd,
																																																amountToAdd,
																																																stack -> player
																																																		.getInventory()
																																																		.setItem(
																																																				5,
																																																				stack))), TOOLBAR_7(
																																																						"toolbar_7",
																																																						Component
																																																								.literal(
																																																										"7"),
																																																						BackpackTranslationHelper.INSTANCE
																																																								.translUpgrade(
																																																										Constants.HOTBAR_TRANSL,
																																																										7)
																																																								.withStyle(
																																																										ChatFormatting.DARK_GREEN),
																																																						(player, playerInvHandler,
																																																								filter) -> getMissingCount(
																																																										player.getInventory()
																																																												.getItem(
																																																														6),
																																																										filter),
																																																						(player, playerInvHandler,
																																																								resourceToAdd,
																																																								amountToAdd) -> refillSlot(
																																																										() -> player
																																																												.getInventory()
																																																												.getItem(
																																																														6),
																																																										resourceToAdd,
																																																										amountToAdd,
																																																										stack -> player
																																																												.getInventory()
																																																												.setItem(
																																																														6,
																																																														stack))), TOOLBAR_8(
																																																																"toolbar_8",
																																																																Component
																																																																		.literal(
																																																																				"8"),
																																																																BackpackTranslationHelper.INSTANCE
																																																																		.translUpgrade(
																																																																				Constants.HOTBAR_TRANSL,
																																																																				8)
																																																																		.withStyle(
																																																																				ChatFormatting.DARK_GREEN),
																																																																(player, playerInvHandler,
																																																																		filter) -> getMissingCount(
																																																																				player.getInventory()
																																																																						.getItem(
																																																																								7),
																																																																				filter),
																																																																(player, playerInvHandler,
																																																																		resourceToAdd,
																																																																		amountToAdd) -> refillSlot(
																																																																				() -> player
																																																																						.getInventory()
																																																																						.getItem(
																																																																								7),
																																																																				resourceToAdd,
																																																																				amountToAdd,
																																																																				stack -> player
																																																																						.getInventory()
																																																																						.setItem(
																																																																								7,
																																																																								stack))), TOOLBAR_9(
																																																																										"toolbar_9",
																																																																										Component
																																																																												.literal(
																																																																														"9"),
																																																																										BackpackTranslationHelper.INSTANCE
																																																																												.translUpgrade(
																																																																														Constants.HOTBAR_TRANSL,
																																																																														9)
																																																																												.withStyle(
																																																																														ChatFormatting.DARK_GREEN),
																																																																										(player, playerInvHandler,
																																																																												filter) -> getMissingCount(
																																																																														player.getInventory()
																																																																																.getItem(
																																																																																		8),
																																																																														filter),
																																																																										(player, playerInvHandler,
																																																																												resourceToAdd,
																																																																												amountToAdd) -> refillSlot(
																																																																														() -> player
																																																																																.getInventory()
																																																																																.getItem(
																																																																																		8),
																																																																														resourceToAdd,
																																																																														amountToAdd,
																																																																														stack -> player
																																																																																.getInventory()
																																																																																.setItem(
																																																																																		8,
																																																																																		stack)));

		private final String name;

		private final Component acronym;
		private final Component description;
		private final MissingCountGetter missingCountGetter;
		private final Filler filler;

		public static final Codec<TargetSlot> CODEC = StringRepresentable.fromEnum(TargetSlot::values);
		public static final StreamCodec<FriendlyByteBuf, TargetSlot> STREAM_CODEC = NeoForgeStreamCodecs.enumCodec(TargetSlot.class);

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
			for (TargetSlot value : values()) {
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
			int getMissingCount(Player player, ResourceHandler<ItemResource> playerInventory, ItemResource filter);
		}

		private interface Filler {
			int fill(Player player, ResourceHandler<ItemResource> playerInventory, ItemResource resourceToAdd, int amountToAdd);
		}

		private static int refillAnywhereInInventory(ResourceHandler<ItemResource> playerInvHandler, ItemResource resourceToAdd, int amountToAdd) {
			AtomicInteger filled = new AtomicInteger(0);
			try (Transaction tx = Transaction.openRoot()) {
				InventoryHelper.iterate(playerInvHandler, (slot, resource, amount) -> {
					if (resource.equals(resourceToAdd)) {
						filled.addAndGet(playerInvHandler.insert(slot, resourceToAdd, amountToAdd - filled.get(), tx));
					}
				}, () -> filled.get() >= amountToAdd);
				if (filled.get() < amountToAdd) {
					filled.addAndGet(playerInvHandler.insert(resourceToAdd, amountToAdd - filled.get(), tx));
				}
				tx.commit();
			}

			return filled.get();
		}

		private static int getMissingCount(ItemStack stack, ItemResource filter) {
			if (filter.matches(stack)) {
				return filter.getMaxStackSize() - stack.getCount();
			}
			return filter.getMaxStackSize();
		}

		private static int refillSlot(Supplier<ItemStack> getSlotContents, ItemResource resourceToAdd, int amountToAdd, Consumer<ItemStack> setSlotContents) {
			ItemStack contents = getSlotContents.get();
			if (contents.isEmpty()) {
				setSlotContents.accept(resourceToAdd.toStack(amountToAdd));
				return amountToAdd;
			}
			if (resourceToAdd.matches(contents)) {
				contents.grow(amountToAdd);
				return amountToAdd;
			}
			return 0;
		}
	}
}
