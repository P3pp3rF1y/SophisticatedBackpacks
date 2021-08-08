package net.p3pp3rf1y.sophisticatedbackpacks.compat.jei;

import mezz.jei.api.gui.IRecipeLayout;
import mezz.jei.api.gui.ingredient.IGuiIngredient;
import mezz.jei.api.gui.ingredient.IGuiItemStackGroup;
import mezz.jei.api.helpers.IStackHelper;
import mezz.jei.api.ingredients.subtypes.UidContext;
import mezz.jei.api.recipe.transfer.IRecipeTransferError;
import mezz.jei.api.recipe.transfer.IRecipeTransferHandler;
import mezz.jei.api.recipe.transfer.IRecipeTransferHandlerHelper;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.inventory.container.Slot;
import net.minecraft.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TranslationHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.ICraftingContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerBase;
import net.p3pp3rf1y.sophisticatedbackpacks.network.PacketHandler;

import javax.annotation.Nullable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.SortedSet;
import java.util.TreeSet;

import static net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks.LOGGER;

public class CraftingContainerRecipeTransferHandler implements IRecipeTransferHandler<BackpackContainer> {
	private final IRecipeTransferHandlerHelper handlerHelper;
	private final IStackHelper stackHelper;

	public CraftingContainerRecipeTransferHandler(IRecipeTransferHandlerHelper handlerHelper, IStackHelper stackHelper) {
		this.handlerHelper = handlerHelper;
		this.stackHelper = stackHelper;
	}

	@Override
	public Class<BackpackContainer> getContainerClass() {
		return BackpackContainer.class;
	}

	@Override
	public IRecipeTransferError transferRecipe(BackpackContainer container, Object recipe, IRecipeLayout recipeLayout, PlayerEntity player, boolean maxTransfer, boolean doTransfer) {
		Optional<? extends UpgradeContainerBase<?, ?>> potentialCraftingContainer = container.getOpenOrFirstCraftingContainer();
		if (!potentialCraftingContainer.isPresent()) {
			return handlerHelper.createInternalError();
		}

		Map<Integer, Slot> inventorySlots = getInventorySlots(container);
		UpgradeContainerBase<?, ?> openOrFirstCraftingContainer = potentialCraftingContainer.get();
		Map<Integer, Slot> craftingSlots = getCraftingSlots((ICraftingContainer) openOrFirstCraftingContainer);
		IGuiItemStackGroup itemStackGroup = recipeLayout.getItemStacks();
		int inputCount = getInputCount(itemStackGroup);

		if (inputCount > craftingSlots.size()) {
			LOGGER.error("Recipe Transfer helper {} does not work for container {}", BackpackContainer.class, container.getClass());
			return handlerHelper.createInternalError();
		}

		Map<Integer, ItemStack> availableItemStacks = new HashMap<>();
		int filledCraftSlotCount = 0;
		for (Slot slot : craftingSlots.values()) {
			ItemStack stack = slot.getItem();
			if (!stack.isEmpty()) {
				if (!slot.mayPickup(player)) {
					LOGGER.error("Recipe Transfer helper {} does not work for container {}. Player can't move item out of Crafting Slot number {}", BackpackContainer.class, container.getClass(), slot.index);
					return handlerHelper.createInternalError();
				}

				++filledCraftSlotCount;
				availableItemStacks.put(slot.index, stack.copy());
			}
		}

		int emptySlotCount = getEmptySlotCount(inventorySlots, availableItemStacks);

		if (filledCraftSlotCount - inputCount > emptySlotCount) {
			String message = TranslationHelper.translate("jei.tooltip.error.recipe.transfer.inventory.full");
			return handlerHelper.createUserErrorWithTooltip(message);
		}

		MatchingItemsResult matchingItemsResult = getMatchingItems(stackHelper, availableItemStacks, itemStackGroup.getGuiIngredients());
		if (!matchingItemsResult.missingItems.isEmpty()) {
			String message = TranslationHelper.translate("jei.tooltip.error.recipe.transfer.missing");
			return handlerHelper.createUserErrorForSlots(message, matchingItemsResult.missingItems);
		}

		List<Integer> craftingSlotIndexes = new ArrayList<>(craftingSlots.keySet());
		Collections.sort(craftingSlotIndexes);
		List<Integer> inventorySlotIndexes = new ArrayList<>(inventorySlots.keySet());
		Collections.sort(inventorySlotIndexes);

		if (doTransfer) {
			if (!openOrFirstCraftingContainer.isOpen()) {
				container.getOpenContainer().ifPresent(c -> {
					c.setIsOpen(false);
					container.setOpenTabId(-1);
				});
				openOrFirstCraftingContainer.setIsOpen(true);
				container.setOpenTabId(openOrFirstCraftingContainer.getUpgradeContainerId());
			}
			TransferRecipeMessage message = new TransferRecipeMessage(matchingItemsResult.matchingItems, craftingSlotIndexes, inventorySlotIndexes, maxTransfer);
			PacketHandler.sendToServer(message);
		}

		return null;
	}

	private int getEmptySlotCount(Map<Integer, Slot> inventorySlots, Map<Integer, ItemStack> availableItemStacks) {
		int emptySlotCount = 0;
		for (Slot slot : inventorySlots.values()) {
			ItemStack stack = slot.getItem();
			if (!stack.isEmpty()) {
				availableItemStacks.put(slot.index, stack.copy());
			} else {
				++emptySlotCount;
			}
		}
		return emptySlotCount;
	}

	private int getInputCount(IGuiItemStackGroup itemStackGroup) {
		int inputCount = 0;
		for (IGuiIngredient<ItemStack> ingredient : itemStackGroup.getGuiIngredients().values()) {
			if (ingredient.isInput() && !ingredient.getAllIngredients().isEmpty()) {
				++inputCount;
			}
		}
		return inputCount;
	}

	private Map<Integer, Slot> getCraftingSlots(ICraftingContainer openOrFirstCraftingContainer) {
		Map<Integer, Slot> craftingSlots = new HashMap<>();
		List<Slot> recipeSlots = openOrFirstCraftingContainer.getRecipeSlots();
		for (Slot slot : recipeSlots) {
			craftingSlots.put(slot.index, slot);
		}
		return craftingSlots;
	}

	private Map<Integer, Slot> getInventorySlots(BackpackContainer container) {
		Map<Integer, Slot> inventorySlots = new HashMap<>();
		for (Slot slot : container.realInventorySlots) {
			inventorySlots.put(slot.index, slot);
		}
		return inventorySlots;
	}

	private MatchingItemsResult getMatchingItems(IStackHelper stackhelper, Map<Integer, ItemStack> availableItemStacks, Map<Integer, ? extends IGuiIngredient<ItemStack>> ingredientsMap) {
		MatchingItemsResult matchingItemResult = new MatchingItemsResult();
		int recipeSlotNumber = -1;
		SortedSet<Integer> ingredientSlots = new TreeSet<>(ingredientsMap.keySet());

		for (int ingredientSlot : ingredientSlots) {
			IGuiIngredient<ItemStack> ingredient = ingredientsMap.get(ingredientSlot);
			if (ingredient.isInput()) {
				++recipeSlotNumber;
				List<ItemStack> requiredStacks = ingredient.getAllIngredients();
				if (!requiredStacks.isEmpty()) {
					tryToMatchStacks(stackhelper, availableItemStacks, matchingItemResult, recipeSlotNumber, ingredientSlot, requiredStacks);
				}
			}
		}

		return matchingItemResult;
	}

	private void tryToMatchStacks(IStackHelper stackhelper, Map<Integer, ItemStack> availableItemStacks, MatchingItemsResult matchingItemResult, int recipeSlotNumber, int ingredientSlot, List<ItemStack> requiredStacks) {
		Integer matching = containsAnyStackIndexed(stackhelper, availableItemStacks, requiredStacks);
		if (matching == null) {
			matchingItemResult.missingItems.add(ingredientSlot);
		} else {
			ItemStack matchingStack = availableItemStacks.get(matching);
			matchingStack.shrink(1);
			if (matchingStack.getCount() == 0) {
				availableItemStacks.remove(matching);
			}

			matchingItemResult.matchingItems.put(recipeSlotNumber, matching);
		}
	}

	@Nullable
	public static Integer containsAnyStackIndexed(IStackHelper stackhelper, Map<Integer, ItemStack> stacks, Iterable<ItemStack> contains) {
		MatchingIndexed matchingStacks = new MatchingIndexed(stacks);
		MatchingIterable matchingContains = new MatchingIterable(contains);
		return containsStackMatchable(stackhelper, matchingStacks, matchingContains);
	}

	@Nullable
	public static <R, T> R containsStackMatchable(IStackHelper stackhelper, Iterable<ItemStackMatchable<R>> stacks, Iterable<ItemStackMatchable<T>> contains) {
		Iterator<ItemStackMatchable<T>> var3 = contains.iterator();

		R matchingStack;
		do {
			if (!var3.hasNext()) {
				return null;
			}

			ItemStackMatchable<T> containStack = var3.next();
			matchingStack = containsStack(stackhelper, stacks, containStack);
		} while (matchingStack == null);

		return matchingStack;
	}

	@Nullable
	public static <R> R containsStack(IStackHelper stackHelper, Iterable<ItemStackMatchable<R>> stacks, ItemStackMatchable<?> contains) {
		Iterator<ItemStackMatchable<R>> var3 = stacks.iterator();

		ItemStackMatchable<R> stack;
		do {
			if (!var3.hasNext()) {
				return null;
			}

			stack = var3.next();
		} while (!stackHelper.isEquivalent(contains.getStack(), stack.getStack(), UidContext.Recipe));

		return stack.getResult();
	}

	private static class MatchingIndexed implements Iterable<ItemStackMatchable<Integer>> {
		private final Map<Integer, ItemStack> map;

		public MatchingIndexed(Map<Integer, ItemStack> map) {
			this.map = map;
		}

		public Iterator<ItemStackMatchable<Integer>> iterator() {
			return new MatchingIterable.DelegateIterator<Map.Entry<Integer, ItemStack>, ItemStackMatchable<Integer>>(map.entrySet().iterator()) {
				public ItemStackMatchable<Integer> next() {
					final Map.Entry<Integer, ItemStack> entry = delegate.next();
					return new ItemStackMatchable<Integer>() {
						public ItemStack getStack() {
							return entry.getValue();
						}

						public Integer getResult() {
							return entry.getKey();
						}
					};
				}
			};
		}
	}

	public static class MatchingIterable implements Iterable<ItemStackMatchable<ItemStack>> {
		private final Iterable<ItemStack> list;

		public MatchingIterable(Iterable<ItemStack> list) {
			this.list = list;
		}

		public Iterator<ItemStackMatchable<ItemStack>> iterator() {
			Iterator<ItemStack> stacks = list.iterator();
			return new MatchingIterable.DelegateIterator<ItemStack, ItemStackMatchable<ItemStack>>(stacks) {
				public ItemStackMatchable<ItemStack> next() {
					final ItemStack stack = delegate.next();
					return new ItemStackMatchable<ItemStack>() {
						@Nullable
						public ItemStack getStack() {
							return stack;
						}

						@Nullable
						public ItemStack getResult() {
							return stack;
						}
					};
				}
			};
		}

		public abstract static class DelegateIterator<T, R> implements Iterator<R> {
			protected final Iterator<T> delegate;

			protected DelegateIterator(Iterator<T> delegate) {
				this.delegate = delegate;
			}

			public boolean hasNext() {
				return delegate.hasNext();
			}

			@Override
			public void remove() {
				delegate.remove();
			}
		}
	}

	public interface ItemStackMatchable<R> {
		@Nullable
		ItemStack getStack();

		@Nullable
		R getResult();
	}

	public static class MatchingItemsResult {
		public final Map<Integer, Integer> matchingItems = new HashMap<>();
		public final List<Integer> missingItems = new ArrayList<>();
	}
}
