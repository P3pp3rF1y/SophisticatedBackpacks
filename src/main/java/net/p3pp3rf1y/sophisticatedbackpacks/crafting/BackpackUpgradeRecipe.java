package net.p3pp3rf1y.sophisticatedbackpacks.crafting;

import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.Container;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.crafting.CraftingBookCategory;
import net.minecraft.world.item.crafting.CraftingInput;
import net.minecraft.world.item.crafting.CraftingRecipe;
import net.minecraft.world.item.crafting.PlacementInfo;
import net.minecraft.world.item.crafting.RecipeSerializer;
import net.minecraft.world.item.crafting.ShapedRecipe;
import net.minecraft.world.level.Level;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackLinkedStorageEndpointAdapter;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackLinkedStorageResolver;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModDataComponents;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.mobcatcher.MobCatcherStorage;
import net.p3pp3rf1y.sophisticatedcore.api.InventoryLayout;
import net.p3pp3rf1y.sophisticatedcore.api.InventoryLayoutFitResult;
import net.p3pp3rf1y.sophisticatedcore.api.InventoryLayoutFitter;
import net.p3pp3rf1y.sophisticatedcore.api.InventoryLayoutPart;
import net.p3pp3rf1y.sophisticatedcore.crafting.IWrapperRecipe;
import net.p3pp3rf1y.sophisticatedcore.crafting.RecipeWrapperSerializer;
import net.p3pp3rf1y.sophisticatedcore.init.ModCoreDataComponents;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageEndpointData;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageEndpointStackState;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageGroupsSavedData;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageStackLifecycle;
import org.jspecify.annotations.Nullable;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public class BackpackUpgradeRecipe implements CraftingRecipe, IWrapperRecipe<ShapedRecipe> {
	public static final RecipeSerializer<BackpackUpgradeRecipe> SERIALIZER = RecipeWrapperSerializer.create(BackpackUpgradeRecipe::new,
			ShapedRecipe.SERIALIZER);
	private final ShapedRecipe compose;

	public BackpackUpgradeRecipe(ShapedRecipe compose) {
		this.compose = compose;
	}

	@Override
	public ShapedRecipe getCompose() {
		return compose;
	}

	@Override
	public boolean matches(CraftingInput input, Level level) {
		return compose.matches(input, level) && getBackpack(input).map(backpack -> canUpgrade(backpack, level)).orElse(false);
	}

	@Override
	public boolean isSpecial() {
		return true;
	}

	@Override
	public ItemStack assemble(CraftingInput inv) {
		ItemStack upgradedBackpack = compose.assemble(inv);
		ItemStack sourceBackpack = getBackpack(inv).orElseThrow();
		upgradedBackpack.applyComponents(sourceBackpack.getComponentsPatch());
		BackpackItem backpackItem = ((BackpackItem) upgradedBackpack.getItem());
		prepareInventoryLayout(sourceBackpack, upgradedBackpack, backpackItem);

		return upgradedBackpack;
	}

	private Optional<ItemStack> getBackpack(CraftingInput inv) {
		for (int slot = 0; slot < inv.size(); slot++) {
			ItemStack slotStack = inv.getItem(slot);
			if (slotStack.getItem() instanceof BackpackItem) {
				return Optional.of(slotStack);
			}
		}

		return Optional.empty();
	}

	static boolean canUpgrade(ItemStack backpack, Level level) {
		if (LinkedStorageStackLifecycle.classifyEndpoint(backpack) != LinkedStorageEndpointStackState.ENDPOINT) {
			return true;
		}
		LinkedStorageEndpointData endpoint = backpack.get(ModCoreDataComponents.LINKED_STORAGE_ENDPOINT);
		if (level instanceof ServerLevel serverLevel) {
			return LinkedStorageGroupsSavedData.get(serverLevel).manager().isPrimaryEndpoint(endpoint.groupId(), endpoint.endpointId());
		}
		return Boolean.TRUE.equals(backpack.get(ModCoreDataComponents.LINKED_STORAGE_PRIMARY_ENDPOINT));
	}

	static void setSlotNumbers(ItemStack backpack, BackpackItem backpackItem) {
		backpack.set(ModCoreDataComponents.NUMBER_OF_INVENTORY_SLOTS, backpackItem.getNumberOfSlots());
		backpack.set(ModCoreDataComponents.NUMBER_OF_UPGRADE_SLOTS, backpackItem.getNumberOfUpgradeSlots());
	}

	static void prepareInventoryLayout(ItemStack sourceBackpack, ItemStack upgradedBackpack, BackpackItem targetBackpackItem) {
		upgradedBackpack.remove(ModDataComponents.TIER_UPGRADE_SOURCE_COLUMNS.get());
		upgradedBackpack.remove(ModDataComponents.TIER_UPGRADE_SOURCE_INVENTORY_SLOTS.get());
		upgradedBackpack.remove(ModDataComponents.TIER_UPGRADE_SOURCE_UPGRADE_SLOTS.get());
		upgradedBackpack.remove(ModDataComponents.TIER_UPGRADE_OPERATION_ID.get());
		if (LinkedStorageStackLifecycle.classifyEndpoint(upgradedBackpack) == LinkedStorageEndpointStackState.ENDPOINT) {
			setSlotNumbers(upgradedBackpack, targetBackpackItem);
			upgradedBackpack.set(ModDataComponents.TIER_UPGRADE_OPERATION_ID.get(), UUID.randomUUID());
			return;
		}

		BackpackItem sourceBackpackItem = (BackpackItem) sourceBackpack.getItem();
		int sourceInventorySlots = getInventorySlots(sourceBackpack, sourceBackpackItem);
		int sourceUpgradeSlots = getUpgradeSlots(sourceBackpack, sourceBackpackItem);
		int columnsTaken = getColumnsTaken(sourceBackpack);
		int sourceColumns = InventoryLayout.getEffectiveColumns(sourceInventorySlots, columnsTaken);
		int targetColumns = InventoryLayout.getEffectiveColumns(targetBackpackItem.getNumberOfSlots(), columnsTaken);
		setSlotNumbers(upgradedBackpack, targetBackpackItem);
		if (sourceColumns != targetColumns) {
			upgradedBackpack.set(ModDataComponents.TIER_UPGRADE_SOURCE_COLUMNS.get(), sourceColumns);
			upgradedBackpack.set(ModDataComponents.TIER_UPGRADE_SOURCE_INVENTORY_SLOTS.get(), sourceInventorySlots);
			upgradedBackpack.set(ModDataComponents.TIER_UPGRADE_SOURCE_UPGRADE_SLOTS.get(), sourceUpgradeSlots);
			upgradedBackpack.set(ModDataComponents.TIER_UPGRADE_OPERATION_ID.get(), UUID.randomUUID());
		}
	}

	public static void completeTierUpgrade(ServerLevel level, Player player, ItemStack callbackStack, @Nullable Container inputs) {
		UUID operationId = callbackStack.get(ModDataComponents.TIER_UPGRADE_OPERATION_ID.get());
		if (operationId == null) {
			return;
		}
		ItemStack upgradedBackpack = findDeliveredStack(player, operationId);
		if (upgradedBackpack.isEmpty() && !callbackStack.isEmpty()) {
			upgradedBackpack = callbackStack;
		}
		if (!upgradedBackpack.isEmpty()) {
			if (LinkedStorageStackLifecycle.classifyEndpoint(upgradedBackpack) == LinkedStorageEndpointStackState.ENDPOINT) {
				completeLinkedTierUpgrade(level, upgradedBackpack, inputs);
			} else {
				Integer sourceColumns = upgradedBackpack.get(ModDataComponents.TIER_UPGRADE_SOURCE_COLUMNS.get());
				Integer sourceInventorySlots = upgradedBackpack.get(ModDataComponents.TIER_UPGRADE_SOURCE_INVENTORY_SLOTS.get());
				Integer sourceUpgradeSlots = upgradedBackpack.get(ModDataComponents.TIER_UPGRADE_SOURCE_UPGRADE_SLOTS.get());
				if (sourceColumns != null && sourceInventorySlots != null && sourceUpgradeSlots != null) {
					InventoryLayoutTarget target = getTargetInventoryLayout(upgradedBackpack, getColumnsTaken(upgradedBackpack));
					if (sourceColumns != target.columns()) {
						IBackpackWrapper source = BackpackWrapper.createDetachedInventoryLayoutView(upgradedBackpack, sourceInventorySlots, sourceUpgradeSlots);
						List<InventoryLayoutPart> parts = source.getInventoryLayoutParts(sourceColumns, target.columns());
						InventoryLayoutFitResult fitResult = InventoryLayoutFitter.fit(parts, target.slots(), target.columns());
						if (fitResult.fits()) {
							IBackpackWrapper wrapper = BackpackWrapper.fromStack(upgradedBackpack);
							wrapper.setSlotNumbers(
									upgradedBackpack.getOrDefault(ModCoreDataComponents.NUMBER_OF_INVENTORY_SLOTS,
											((BackpackItem) upgradedBackpack.getItem()).getNumberOfSlots()),
									upgradedBackpack.getOrDefault(ModCoreDataComponents.NUMBER_OF_UPGRADE_SLOTS,
											((BackpackItem) upgradedBackpack.getItem()).getNumberOfUpgradeSlots()));
							expandInventoryForTierUpgrade(wrapper, upgradedBackpack, target.slots());
							wrapper.applyInventoryLayout(fitResult, target.columns());
						}
					}
				}
			}
		}
		upgradedBackpack.remove(ModDataComponents.TIER_UPGRADE_SOURCE_COLUMNS.get());
		upgradedBackpack.remove(ModDataComponents.TIER_UPGRADE_SOURCE_INVENTORY_SLOTS.get());
		upgradedBackpack.remove(ModDataComponents.TIER_UPGRADE_SOURCE_UPGRADE_SLOTS.get());
		upgradedBackpack.remove(ModDataComponents.TIER_UPGRADE_OPERATION_ID.get());
		callbackStack.remove(ModDataComponents.TIER_UPGRADE_SOURCE_COLUMNS.get());
		callbackStack.remove(ModDataComponents.TIER_UPGRADE_SOURCE_INVENTORY_SLOTS.get());
		callbackStack.remove(ModDataComponents.TIER_UPGRADE_SOURCE_UPGRADE_SLOTS.get());
		callbackStack.remove(ModDataComponents.TIER_UPGRADE_OPERATION_ID.get());
	}

	private static void expandInventoryForTierUpgrade(IBackpackWrapper wrapper, ItemStack upgradedBackpack, int targetSlots) {
		int inventorySlots = upgradedBackpack.getOrDefault(ModCoreDataComponents.NUMBER_OF_INVENTORY_SLOTS,
				((BackpackItem) upgradedBackpack.getItem()).getNumberOfSlots());
		int currentSlots = wrapper.getInventoryHandler().size();
		if (targetSlots > currentSlots) {
			wrapper.setSlotNumbers(inventorySlots, upgradedBackpack.getOrDefault(ModCoreDataComponents.NUMBER_OF_UPGRADE_SLOTS,
					((BackpackItem) upgradedBackpack.getItem()).getNumberOfUpgradeSlots()));
		}
	}

	private static ItemStack findDeliveredStack(Player player, UUID operationId) {
		ItemStack carried = player.containerMenu.getCarried();
		if (operationId.equals(carried.get(ModDataComponents.TIER_UPGRADE_OPERATION_ID.get()))) {
			return carried;
		}
		for (ItemStack stack : player.getInventory().getNonEquipmentItems()) {
			if (operationId.equals(stack.get(ModDataComponents.TIER_UPGRADE_OPERATION_ID.get()))) {
				return stack;
			}
		}
		return ItemStack.EMPTY;
	}

	private static void completeLinkedTierUpgrade(ServerLevel level, ItemStack upgradedBackpack, @Nullable Container inputs) {
		IBackpackWrapper source = BackpackLinkedStorageResolver.resolvePrimaryCanonicalHost(level, upgradedBackpack).orElse(null);
		if (source == null) {
			return;
		}
		int sourceColumns = MobCatcherStorage.getColumns(source);
		InventoryLayoutTarget target = getTargetInventoryLayout(upgradedBackpack, source.getColumnsTaken());
		boolean layoutChanged = sourceColumns != target.columns();
		List<InventoryLayoutPart> parts = layoutChanged ? source.getInventoryLayoutParts(sourceColumns, target.columns()) : List.of();
		InventoryLayoutFitResult fitResult = layoutChanged ? InventoryLayoutFitter.fit(parts, target.slots(), target.columns()) : null;
		if (layoutChanged && !fitResult.fits()) {
			return;
		}
		boolean completed = inputs == null
				? BackpackLinkedStorageEndpointAdapter.completePrimaryTierUpgrade(level, upgradedBackpack)
				: BackpackLinkedStorageEndpointAdapter.completePrimaryTierUpgrade(level, upgradedBackpack, inputs);
		if (completed && layoutChanged) {
			BackpackLinkedStorageResolver.resolvePrimaryCanonicalHost(level, upgradedBackpack).ifPresent(canonicalTarget -> {
				canonicalTarget.applyInventoryLayout(fitResult, target.columns());
			});
		}
	}

	private static InventoryLayoutTarget getTargetInventoryLayout(ItemStack upgradedBackpack, int columnsTaken) {
		int inventorySlots = upgradedBackpack.getOrDefault(ModCoreDataComponents.NUMBER_OF_INVENTORY_SLOTS,
				((BackpackItem) upgradedBackpack.getItem()).getNumberOfSlots());
		return new InventoryLayoutTarget(InventoryLayout.getEffectiveColumns(inventorySlots, columnsTaken),
				InventoryLayout.getUsableInventorySlots(inventorySlots, columnsTaken));
	}

	private record InventoryLayoutTarget(int columns, int slots) {
	}

	private static int getInventorySlots(ItemStack backpack, BackpackItem backpackItem) {
		Integer storedSlots = backpack.get(ModCoreDataComponents.NUMBER_OF_INVENTORY_SLOTS);
		return storedSlots != null && !Config.SERVER_SPEC.isLoaded()
				? storedSlots
				: Math.max(storedSlots == null ? backpackItem.getNumberOfSlots() : storedSlots, backpackItem.getNumberOfSlots());
	}

	private static int getColumnsTaken(ItemStack backpack) {
		return backpack.getOrDefault(ModDataComponents.COLUMNS_TAKEN.get(), 0);
	}

	private static int getUpgradeSlots(ItemStack backpack, BackpackItem backpackItem) {
		Integer storedSlots = backpack.get(ModCoreDataComponents.NUMBER_OF_UPGRADE_SLOTS);
		return storedSlots != null && !Config.SERVER_SPEC.isLoaded()
				? storedSlots
				: Math.max(storedSlots == null ? backpackItem.getNumberOfUpgradeSlots() : storedSlots, backpackItem.getNumberOfUpgradeSlots());
	}

	@Override
	public RecipeSerializer<BackpackUpgradeRecipe> getSerializer() {
		return ModItems.BACKPACK_UPGRADE_RECIPE_SERIALIZER.get();
	}

	@Override
	public boolean showNotification() {
		return compose.showNotification();
	}

	@Override
	public String group() {
		return compose.group();
	}

	@Override
	public CraftingBookCategory category() {
		return compose.category();
	}

	@Override
	public PlacementInfo placementInfo() {
		return compose.placementInfo();
	}

	@Override
	public List<net.minecraft.world.item.crafting.display.RecipeDisplay> display() {
		return compose.display();
	}

}
