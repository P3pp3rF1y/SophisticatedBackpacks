package net.p3pp3rf1y.sophisticatedbackpacks.compat.craftingtweaks;

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.HashMultiset;
import com.google.common.collect.Multiset;
import net.blay09.mods.craftingtweaks.api.CraftingGrid;
import net.blay09.mods.craftingtweaks.api.CraftingGridBuilder;
import net.blay09.mods.craftingtweaks.api.CraftingGridProvider;
import net.blay09.mods.craftingtweaks.api.GridTransferHandler;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.Container;
import net.minecraft.world.SimpleContainer;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.AbstractContainerMenu;
import net.minecraft.world.inventory.Slot;
import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;
import net.p3pp3rf1y.sophisticatedcore.common.gui.ICraftingContainer;

import java.util.List;
import java.util.Objects;
import java.util.Optional;

@SuppressWarnings("java:S3776") //keeping this as close as possible to default implementation in crafting tweaks hence higher complexity but easier porting
public class CraftingUpgradeTweakProvider implements CraftingGridProvider {

	@Override
	public String getModId() {
		return SophisticatedBackpacks.MOD_ID;
	}

	@Override
	public boolean handles(AbstractContainerMenu abstractContainerMenu) {
		return abstractContainerMenu instanceof BackpackContainer;
	}

	@Override
	public void buildCraftingGrids(CraftingGridBuilder builder, AbstractContainerMenu containerMenu) {
		if (!(containerMenu instanceof BackpackContainer backpackContainer)) {
			return;
		}
		builder.addGrid(getCraftingGridStart(backpackContainer), getCraftingGridSize(backpackContainer))
				.clearHandler((craftingGrid, player, menu, forced) -> clearGrid(player, menu, forced))
				.rotateHandler((craftingGrid, player, menu, reverse) -> rotateGrid(menu, reverse))
				.balanceHandler(new BackpackCraftingGridBalanceHandler())
				.transferHandler(new BackpackCraftingGridTransferHandler())
				.hideAllTweakButtons();
	}

	public void clearGrid(Player player, AbstractContainerMenu menu, boolean forced) {
		if (!(menu instanceof BackpackContainer backpackContainer)) {
			return;
		}

		getCraftMatrix(backpackContainer).ifPresent(craftMatrix -> {
			int start = getCraftingGridStart(backpackContainer);
			int size = getCraftingGridSize(backpackContainer);

			for (int i = start; i < start + size; ++i) {
				int slotIndex = (backpackContainer.getSlot(i)).getContainerSlot();
				ItemStack itemStack = craftMatrix.getItem(slotIndex);
				if (!itemStack.isEmpty()) {
					ItemStack returnStack = itemStack.copy();
					player.getInventory().add(returnStack);
					craftMatrix.setItem(slotIndex, returnStack.getCount() == 0 ? ItemStack.EMPTY : returnStack);
					if (returnStack.getCount() > 0 && forced) {
						player.drop(returnStack, false);
						craftMatrix.setItem(slotIndex, ItemStack.EMPTY);
					}
				}
			}

			backpackContainer.broadcastChanges();
		});
	}

	private int rotateSlotId(int slotId, boolean counterClockwise) {
		if (!counterClockwise) {
			switch (slotId) {
				case 0:
					return 1;
				case 1:
					return 2;
				case 2:
					return 5;
				case 3:
					return 0;
				case 5:
					return 8;
				case 6:
					return 3;
				case 7:
					return 6;
				case 8:
					return 7;
				default:
					break;
			}
		} else {
			switch (slotId) {
				case 0:
					return 3;
				case 1:
					return 0;
				case 2:
					return 1;
				case 3:
					return 6;
				case 5:
					return 2;
				case 6:
					return 7;
				case 7:
					return 8;
				case 8:
					return 5;
				default:
					break;
			}
		}

		return 0;
	}

	private boolean ignoresSlotId(int slotId) {
		return slotId == 4;
	}

	private void rotateGrid(AbstractContainerMenu containerMenu, boolean counterClockwise) {
		if (!(containerMenu instanceof BackpackContainer backpackContainer)) {
			return;
		}
		getCraftMatrix(backpackContainer).ifPresent(craftMatrix -> {
			int start = getCraftingGridStart(backpackContainer);
			int size = getCraftingGridSize(backpackContainer);
			Container matrixClone = new SimpleContainer(size);

			int i;
			int slotIndex;
			for (i = 0; i < size; ++i) {
				slotIndex = backpackContainer.getSlot(start + i).getContainerSlot();
				matrixClone.setItem(i, craftMatrix.getItem(slotIndex));
			}

			for (i = 0; i < size; ++i) {
				if (!ignoresSlotId(i)) {
					slotIndex = containerMenu.getSlot(start + rotateSlotId(i, counterClockwise)).getContainerSlot();
					craftMatrix.setItem(slotIndex, matrixClone.getItem(i));
				}
			}

			backpackContainer.broadcastChanges();
		});
	}

	private static Optional<Container> getCraftMatrix(BackpackContainer container) {
		return getOpenCraftingContainer(container).map(ICraftingContainer::getCraftMatrix);
	}

	@Override
	public boolean requiresServerSide() {
		return true;
	}

	private static Optional<ICraftingContainer> getOpenCraftingContainer(BackpackContainer container) {
		return container.getOpenContainer().flatMap(c -> (c instanceof ICraftingContainer craftingContainer) ? Optional.of(craftingContainer) : Optional.empty());
	}

	private static int getCraftingGridStart(BackpackContainer container) {
		return getOpenCraftingContainer(container).map(cc -> {
			List<Slot> recipeSlots = cc.getRecipeSlots();
			if (!recipeSlots.isEmpty()) {
				return recipeSlots.get(0).index;
			}
			return 0;
		}).orElse(0);
	}

	private static int getCraftingGridSize(BackpackContainer container) {
		return getOpenCraftingContainer(container).isPresent() ? 9 : 0;
	}

	private static class BackpackCraftingGridBalanceHandler implements net.blay09.mods.craftingtweaks.api.GridBalanceHandler<AbstractContainerMenu> {
		@Override
		public void balanceGrid(CraftingGrid grid, Player player, AbstractContainerMenu menu) {
			if (!(menu instanceof BackpackContainer backpackContainer)) {
				return;
			}
			getCraftMatrix(backpackContainer).ifPresent(craftMatrix -> {
				ArrayListMultimap<String, ItemStack> itemMap = ArrayListMultimap.create();
				Multiset<String> itemCount = HashMultiset.create();
				int start = getCraftingGridStart(backpackContainer);
				int size = getCraftingGridSize(backpackContainer);
				for (int i = start; i < start + size; i++) {
					int slotIndex = menu.getSlot(i).getContainerSlot();
					ItemStack itemStack = craftMatrix.getItem(slotIndex);
					if (!itemStack.isEmpty() && itemStack.getMaxStackSize() > 1) {
						ResourceLocation registryName = itemStack.getItem().getRegistryName();
						String key = Objects.toString(registryName);
						itemMap.put(key, itemStack);
						itemCount.add(key, itemStack.getCount());
					}
				}

				for (String key : itemMap.keySet()) {
					List<ItemStack> balanceList = itemMap.get(key);
					int totalCount = itemCount.count(key);
					int countPerStack = totalCount / balanceList.size();
					int restCount = totalCount % balanceList.size();
					for (ItemStack itemStack : balanceList) {
						itemStack.setCount(countPerStack);
					}

					int idx = 0;
					while (restCount > 0) {
						ItemStack itemStack = balanceList.get(idx);
						if (itemStack.getCount() < itemStack.getMaxStackSize()) {
							itemStack.grow(1);
							restCount--;
						}
						idx++;
						if (idx >= balanceList.size()) {
							idx = 0;
						}
					}
				}

				menu.broadcastChanges();
			});
		}

		@Override
		public void spreadGrid(CraftingGrid grid, Player player, AbstractContainerMenu menu) {
			if (!(menu instanceof BackpackContainer backpackContainer)) {
				return;
			}
			getCraftMatrix(backpackContainer).ifPresent(craftMatrix -> {
				while (true) {
					ItemStack biggestSlotStack = null;
					int biggestSlotSize = 1;
					int start = getCraftingGridStart(backpackContainer);
					int size = getCraftingGridSize(backpackContainer);
					for (int i = start; i < start + size; i++) {
						int slotIndex = menu.getSlot(i).getContainerSlot();
						ItemStack itemStack = craftMatrix.getItem(slotIndex);
						if (!itemStack.isEmpty() && itemStack.getCount() > biggestSlotSize) {
							biggestSlotStack = itemStack;
							biggestSlotSize = itemStack.getCount();
						}
					}

					if (biggestSlotStack == null) {
						return;
					}

					boolean emptyBiggestSlot = false;
					for (int i = start; i < start + size; i++) {
						int slotIndex = menu.getSlot(i).getContainerSlot();
						ItemStack itemStack = craftMatrix.getItem(slotIndex);
						if (itemStack.isEmpty()) {
							if (biggestSlotStack.getCount() > 1) {
								craftMatrix.setItem(slotIndex, biggestSlotStack.split(1));
							} else {
								emptyBiggestSlot = true;
							}
						}
					}

					if (!emptyBiggestSlot) {
						break;
					}
				}

				balanceGrid(grid, player, menu);
			});
		}
	}

	private static class BackpackCraftingGridTransferHandler implements GridTransferHandler<AbstractContainerMenu> {
		@Override
		public ItemStack putIntoGrid(CraftingGrid craftingGrid, Player player, AbstractContainerMenu menu, int slotId, ItemStack itemStack) {
			if (!(menu instanceof BackpackContainer backpackContainer)) {
				return itemStack;
			}
			return getCraftMatrix(backpackContainer).map(craftMatrix -> {
				ItemStack craftStack = craftMatrix.getItem(slotId);
				if (!craftStack.isEmpty()) {
					if (craftStack.sameItem(itemStack) && ItemStack.tagMatches(craftStack, itemStack)) {
						int spaceLeft = Math.min(craftMatrix.getMaxStackSize(), craftStack.getMaxStackSize()) - craftStack.getCount();
						if (spaceLeft > 0) {
							ItemStack splitStack = itemStack.split(Math.min(spaceLeft, itemStack.getCount()));
							craftStack.grow(splitStack.getCount());
							if (itemStack.getCount() <= 0) {
								return ItemStack.EMPTY;
							}
						}
					}
				} else {
					ItemStack transferStack = itemStack.split(Math.min(itemStack.getCount(), craftMatrix.getMaxStackSize()));
					craftMatrix.setItem(slotId, transferStack);
				}

				return itemStack.getCount() <= 0 ? ItemStack.EMPTY : itemStack;
			}).orElse(itemStack);
		}

		@Override
		public boolean transferIntoGrid(CraftingGrid craftingGrid, Player player, AbstractContainerMenu menu, Slot fromSlot) {
			if (!(menu instanceof BackpackContainer backpackContainer)) {
				return false;
			}
			return getCraftMatrix(backpackContainer).map(craftMatrix -> {
				int start = getCraftingGridStart(backpackContainer);
				int size = getCraftingGridSize(backpackContainer);
				ItemStack itemStack = fromSlot.getItem();
				if (itemStack.isEmpty()) {
					return false;
				} else {
					int firstEmptySlot = -1;

					for (int i = start; i < start + size; ++i) {
						int slotIndex = menu.getSlot(i).getContainerSlot();
						ItemStack craftStack = craftMatrix.getItem(slotIndex);
						if (!craftStack.isEmpty()) {
							if (craftStack.sameItem(itemStack) && ItemStack.tagMatches(craftStack, itemStack)) {
								int spaceLeft = Math.min(craftMatrix.getMaxStackSize(), craftStack.getMaxStackSize()) - craftStack.getCount();
								if (spaceLeft > 0) {
									ItemStack splitStack = itemStack.split(Math.min(spaceLeft, itemStack.getCount()));
									craftStack.grow(splitStack.getCount());
									if (itemStack.getCount() <= 0) {
										return true;
									}
								}
							}
						} else if (firstEmptySlot == -1) {
							firstEmptySlot = slotIndex;
						}
					}

					if (itemStack.getCount() > 0 && firstEmptySlot != -1) {
						ItemStack transferStack = itemStack.split(Math.min(itemStack.getCount(), craftMatrix.getMaxStackSize()));
						craftMatrix.setItem(firstEmptySlot, transferStack);
						return true;
					} else {
						return false;
					}
				}
			}).orElse(false);
		}

		@Override
		public boolean canTransferFrom(Player player, AbstractContainerMenu menu, Slot sourceSlot, CraftingGrid craftingGrid) {
			if (!(menu instanceof BackpackContainer backpackContainer)) {
				return false;
			}
			return sourceSlot.mayPickup(player) && sourceSlot.index < backpackContainer.realInventorySlots.size();
		}
	}
}
