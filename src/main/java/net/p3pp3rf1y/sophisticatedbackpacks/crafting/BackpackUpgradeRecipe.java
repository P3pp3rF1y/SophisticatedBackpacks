package net.p3pp3rf1y.sophisticatedbackpacks.crafting;

import net.minecraft.core.RegistryAccess;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.Container;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.CraftingContainer;
import net.minecraft.world.item.ItemStack;
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
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageEndpointData;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageEndpointStackState;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageGroupsSavedData;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageStackData;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageStackLifecycle;

import javax.annotation.Nullable;

import java.util.LinkedHashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;

public class BackpackUpgradeRecipe extends ShapedRecipe implements IWrapperRecipe<ShapedRecipe> {
	public static final Set<ResourceLocation> REGISTERED_RECIPES = new LinkedHashSet<>();
	private final ShapedRecipe compose;

	public BackpackUpgradeRecipe(ShapedRecipe compose) {
		super(compose.getId(), compose.getGroup(), compose.category(), compose.getRecipeWidth(), compose.getRecipeHeight(), compose.getIngredients(),
				compose.result);
		this.compose = compose;
		REGISTERED_RECIPES.add(compose.getId());
	}

	@Override
	public ShapedRecipe getCompose() {
		return compose;
	}

	@Override
	public boolean isSpecial() {
		return true;
	}

	@Override
	public boolean matches(CraftingContainer inv, Level level) {
		return super.matches(inv, level) && getBackpack(inv).map(backpack -> canUpgrade(backpack, level)).orElse(false);
	}

	static boolean canUpgrade(ItemStack backpack, Level level) {
		LinkedStorageEndpointData endpoint = LinkedStorageStackData.getEndpoint(backpack);
		if (endpoint == null) {
			return true;
		}
		return level instanceof ServerLevel serverLevel
				? LinkedStorageGroupsSavedData.get(serverLevel).manager().isPrimaryEndpoint(endpoint.groupId(), endpoint.endpointId())
				: LinkedStorageStackData.isPrimaryEndpoint(backpack);
	}

	@Override
	public ItemStack assemble(CraftingContainer inv, RegistryAccess registryAccess) {
		ItemStack upgradedBackpack = super.assemble(inv, registryAccess);
		ItemStack sourceBackpack = getBackpack(inv).orElseThrow();
		if (sourceBackpack.getTag() != null) {
			upgradedBackpack.setTag(sourceBackpack.getTag().copy());
		}
		BackpackItem backpackItem = ((BackpackItem) upgradedBackpack.getItem());
		prepareInventoryLayout(sourceBackpack, upgradedBackpack, backpackItem);

		return upgradedBackpack;
	}

	static void prepareInventoryLayout(ItemStack sourceBackpack, ItemStack upgradedBackpack, BackpackItem targetBackpackItem) {
		ModDataComponents.clearTierUpgrade(upgradedBackpack);
		setSlotNumbers(upgradedBackpack, targetBackpackItem);
		if (LinkedStorageStackLifecycle.classifyEndpoint(upgradedBackpack) == LinkedStorageEndpointStackState.ENDPOINT) {
			ModDataComponents.setOperationId(upgradedBackpack, UUID.randomUUID());
			return;
		}

		BackpackItem sourceBackpackItem = (BackpackItem) sourceBackpack.getItem();
		int sourceInventorySlots = getInventorySlots(sourceBackpack, sourceBackpackItem);
		int sourceUpgradeSlots = getUpgradeSlots(sourceBackpack, sourceBackpackItem);
		int columnsTaken = getColumnsTaken(sourceBackpack);
		int sourceColumns = InventoryLayout.getEffectiveColumns(sourceInventorySlots, columnsTaken);
		int targetColumns = InventoryLayout.getEffectiveColumns(targetBackpackItem.getNumberOfSlots(), columnsTaken);
		if (sourceColumns != targetColumns) {
			ModDataComponents.setSourceColumns(upgradedBackpack, sourceColumns);
			ModDataComponents.setSourceInventorySlots(upgradedBackpack, sourceInventorySlots);
			ModDataComponents.setSourceUpgradeSlots(upgradedBackpack, sourceUpgradeSlots);
			ModDataComponents.setOperationId(upgradedBackpack, UUID.randomUUID());
		}
	}

	static void setSlotNumbers(ItemStack backpack, BackpackItem backpackItem) {
		backpack.getOrCreateTag().putInt("inventorySlots", backpackItem.getNumberOfSlots());
		backpack.getOrCreateTag().putInt("upgradeSlots", backpackItem.getNumberOfUpgradeSlots());
	}

	public static void completeTierUpgrade(ServerLevel level, Player player, ItemStack callbackStack, @Nullable Container inputs) {
		UUID operationId = ModDataComponents.getOperationId(callbackStack);
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
				Integer sourceColumns = ModDataComponents.getSourceColumns(upgradedBackpack);
				Integer sourceInventorySlots = ModDataComponents.getSourceInventorySlots(upgradedBackpack);
				Integer sourceUpgradeSlots = ModDataComponents.getSourceUpgradeSlots(upgradedBackpack);
				if (sourceColumns != null && sourceInventorySlots != null && sourceUpgradeSlots != null) {
					InventoryLayoutTarget target = getTargetInventoryLayout(upgradedBackpack, getColumnsTaken(upgradedBackpack));
					if (sourceColumns != target.columns()) {
						IBackpackWrapper source = BackpackWrapper.createDetachedInventoryLayoutView(upgradedBackpack, sourceInventorySlots, sourceUpgradeSlots);
						List<InventoryLayoutPart> parts = source.getInventoryLayoutParts(sourceColumns, target.columns());
						InventoryLayoutFitResult fitResult = InventoryLayoutFitter.fit(parts, target.slots(), target.columns());
						if (fitResult.fits()) {
							IBackpackWrapper wrapper = BackpackLinkedStorageResolver.resolveOrCreate(level, upgradedBackpack);
							wrapper.setSlotNumbers(getInventorySlots(upgradedBackpack, (BackpackItem) upgradedBackpack.getItem()),
									getUpgradeSlots(upgradedBackpack, (BackpackItem) upgradedBackpack.getItem()));
							expandInventoryForTierUpgrade(wrapper, target.slots());
							wrapper.applyInventoryLayout(fitResult, target.columns());
						}
					}
				}
			}
		}
		ModDataComponents.clearTierUpgrade(upgradedBackpack);
		ModDataComponents.clearTierUpgrade(callbackStack);
	}

	private static ItemStack findDeliveredStack(Player player, UUID operationId) {
		ItemStack carried = player.containerMenu.getCarried();
		if (operationId.equals(ModDataComponents.getOperationId(carried))) {
			return carried;
		}
		for (ItemStack stack : player.getInventory().items) {
			if (operationId.equals(ModDataComponents.getOperationId(stack))) {
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

	private static void expandInventoryForTierUpgrade(IBackpackWrapper wrapper, int targetSlots) {
		int currentSlots = wrapper.getInventoryHandler().getSlots();
		if (targetSlots > currentSlots) {
			wrapper.getInventoryHandler().changeSlots(targetSlots - currentSlots);
		}
	}

	private static InventoryLayoutTarget getTargetInventoryLayout(ItemStack upgradedBackpack, int columnsTaken) {
		int inventorySlots = getInventorySlots(upgradedBackpack, (BackpackItem) upgradedBackpack.getItem());
		return new InventoryLayoutTarget(InventoryLayout.getEffectiveColumns(inventorySlots, columnsTaken),
				InventoryLayout.getUsableInventorySlots(inventorySlots, columnsTaken));
	}

	private record InventoryLayoutTarget(int columns, int slots) {
	}

	private static int getInventorySlots(ItemStack backpack, BackpackItem backpackItem) {
		Integer storedSlots = backpack.getTag() != null && backpack.getTag().contains("inventorySlots") ? backpack.getTag().getInt("inventorySlots") : null;
		return storedSlots != null && !Config.SERVER_SPEC.isLoaded()
				? storedSlots
				: Math.max(storedSlots == null ? backpackItem.getNumberOfSlots() : storedSlots, backpackItem.getNumberOfSlots());
	}

	private static int getUpgradeSlots(ItemStack backpack, BackpackItem backpackItem) {
		Integer storedSlots = backpack.getTag() != null && backpack.getTag().contains("upgradeSlots") ? backpack.getTag().getInt("upgradeSlots") : null;
		return storedSlots != null && !Config.SERVER_SPEC.isLoaded()
				? storedSlots
				: Math.max(storedSlots == null ? backpackItem.getNumberOfUpgradeSlots() : storedSlots, backpackItem.getNumberOfUpgradeSlots());
	}

	private static int getColumnsTaken(ItemStack backpack) {
		return backpack.getTag() != null && backpack.getTag().contains("columnsTaken") ? backpack.getTag().getInt("columnsTaken") : 0;
	}

	private Optional<ItemStack> getBackpack(CraftingContainer inv) {
		for (int slot = 0; slot < inv.getContainerSize(); slot++) {
			ItemStack slotStack = inv.getItem(slot);
			if (slotStack.getItem() instanceof BackpackItem) {
				return Optional.of(slotStack);
			}
		}

		return Optional.empty();
	}

	@Override
	public RecipeSerializer<?> getSerializer() {
		return ModItems.BACKPACK_UPGRADE_RECIPE_SERIALIZER.get();
	}

	public static class Serializer extends RecipeWrapperSerializer<ShapedRecipe, BackpackUpgradeRecipe> {
		public Serializer() {
			super(BackpackUpgradeRecipe::new, RecipeSerializer.SHAPED_RECIPE);
		}
	}
}
