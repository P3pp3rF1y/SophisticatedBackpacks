package net.p3pp3rf1y.sophisticatedbackpacks.compat.craftingtweaks;

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.HashMultiset;
import com.google.common.collect.Multiset;
import net.blay09.mods.craftingtweaks.api.CraftingTweaksAPI;
import net.blay09.mods.craftingtweaks.api.DefaultProviderV2;
import net.blay09.mods.craftingtweaks.api.RotationHandler;
import net.blay09.mods.craftingtweaks.api.TweakProvider;
import net.minecraft.client.gui.screen.inventory.ContainerScreen;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.inventory.IInventory;
import net.minecraft.inventory.Inventory;
import net.minecraft.inventory.container.Container;
import net.minecraft.inventory.container.Slot;
import net.minecraft.item.ItemStack;
import net.minecraft.util.ResourceLocation;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import net.minecraftforge.client.event.GuiScreenEvent;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.ICraftingContainer;

import javax.annotation.Nullable;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

public class CraftingUpgradeTweakProvider implements TweakProvider<BackpackContainer> {
	private static final RotationHandler ROTATION_HANDLER = new RotationHandler() {
		public boolean ignoreSlotId(int slotId) {
			return slotId == 4;
		}

		public int rotateSlotId(int slotId, boolean counterClockwise) {
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
					case 4:
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
					case 4:
					default:
						break;
				}
			}
			return 0;
		}
	};

	private final DefaultProviderV2 defaultProvider = CraftingTweaksAPI.createDefaultProviderV2();

	@Override
	public String getModId() {
		return SophisticatedBackpacks.MOD_ID;
	}

	@Override
	public boolean load() {
		return true;
	}

	@Override
	public void clearGrid(PlayerEntity entityPlayer, BackpackContainer container, int id, boolean forced) {
		IInventory craftMatrix = getCraftMatrix(entityPlayer, container, id);
		if (craftMatrix != null) {
			int start = getCraftingGridStart(entityPlayer, container, id);
			int size = getCraftingGridSize(entityPlayer, container, id);

			for (int slotNumber = start; slotNumber < start + size; ++slotNumber) {
				Slot slot = container.getSlot(slotNumber);
				int slotIndex = slot.getSlotIndex();
				container.quickMoveStack(entityPlayer, slotNumber);
				if (slot.hasItem() && forced) {
					entityPlayer.drop(slot.getItem(), false);
					craftMatrix.setItem(slotIndex, ItemStack.EMPTY);
				}
			}
			container.sendSlotUpdates();
		}
	}

	@Override
	public void rotateGrid(PlayerEntity entityPlayer, BackpackContainer container, int id, boolean counterClockwise) {
		IInventory craftMatrix = getCraftMatrix(entityPlayer, container, id);
		if (craftMatrix != null) {
			int start = getCraftingGridStart(entityPlayer, container, id);
			int size = getCraftingGridSize(entityPlayer, container, id);
			IInventory matrixClone = new Inventory(size);

			int i;
			int slotIndex;
			for (i = 0; i < size; ++i) {
				slotIndex = container.getSlot(start + i).getSlotIndex();
				matrixClone.setItem(i, craftMatrix.getItem(slotIndex));
			}

			for (i = 0; i < size; ++i) {
				if (!ROTATION_HANDLER.ignoreSlotId(i)) {
					slotIndex = container.getSlot(start + ROTATION_HANDLER.rotateSlotId(i, counterClockwise)).getSlotIndex();
					craftMatrix.setItem(slotIndex, matrixClone.getItem(i));
				}
			}

			container.sendSlotUpdates();
		}
	}

	@Override
	public void balanceGrid(PlayerEntity entityPlayer, BackpackContainer container, int id) {
		ArrayListMultimap<String, ItemStack> itemMap = ArrayListMultimap.create();
		Multiset<String> itemCount = HashMultiset.create();
		int start = getCraftingGridStart(entityPlayer, container, id);
		int size = getCraftingGridSize(entityPlayer, container, id);
		for (int i = start; i < start + size; i++) {
			ItemStack itemStack = container.getSlot(i).getItem();
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

		container.sendSlotUpdates();
	}

	@Override
	public void spreadGrid(PlayerEntity entityPlayer, BackpackContainer container, int id) {
		while (true) {
			ItemStack biggestSlotStack = null;
			int biggestSlotSize = 1;
			int start = getCraftingGridStart(entityPlayer, container, id);
			int size = getCraftingGridSize(entityPlayer, container, id);
			for (int i = start; i < start + size; i++) {
				ItemStack itemStack = container.getSlot(i).getItem();
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
				Slot slot = container.getSlot(i);
				ItemStack itemStack = slot.getItem();
				if (itemStack.isEmpty()) {
					if (biggestSlotStack.getCount() > 1) {
						slot.set(biggestSlotStack.split(1));
					} else {
						emptyBiggestSlot = true;
					}
				}
			}

			if (!emptyBiggestSlot) {
				break;
			}
		}
		balanceGrid(this, id, entityPlayer, container);
	}

	private <T extends BackpackContainer> void balanceGrid(TweakProvider<T> provider, int id, PlayerEntity entityPlayer, T container) {
		ArrayListMultimap<String, ItemStack> itemMap = ArrayListMultimap.create();
		Multiset<String> itemCount = HashMultiset.create();
		int start = provider.getCraftingGridStart(entityPlayer, container, id);
		int size = provider.getCraftingGridSize(entityPlayer, container, id);
		for (int slotNumber = start; slotNumber < start + size; slotNumber++) {
			ItemStack itemStack = container.getSlot(slotNumber).getItem();
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

		container.sendSlotUpdates();
	}

	@Override
	public boolean canTransferFrom(PlayerEntity entityPlayer, BackpackContainer container, int id, Slot sourceSlot) {
		return sourceSlot.mayPickup(entityPlayer) && sourceSlot.index < container.realInventorySlots.size();
	}

	@Override
	public boolean transferIntoGrid(PlayerEntity entityPlayer, BackpackContainer container, int id, Slot sourceSlot) {
		IInventory craftMatrix = getCraftMatrix(entityPlayer, container, id);
		if (craftMatrix == null) {
			return false;
		}

		int start = getCraftingGridStart(entityPlayer, container, id);
		int size = getCraftingGridSize(entityPlayer, container, id);
		ItemStack itemStack = sourceSlot.getItem();
		if (itemStack.isEmpty()) {
			return false;
		}

		int firstEmptySlot = -1;
		for (int i = start; i < start + size; i++) {
			int slotIndex = container.getSlot(i).getSlotIndex();
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
			container.sendSlotUpdates();
			return true;
		}

		return false;
	}

	@Override
	public ItemStack putIntoGrid(PlayerEntity entityPlayer, BackpackContainer container, int id, ItemStack itemStack, int index) {
		return defaultProvider.putIntoGrid(this, id, entityPlayer, container, itemStack, index);
	}

	@Override
	@Nullable
	public IInventory getCraftMatrix(PlayerEntity entityPlayer, BackpackContainer container, int id) {
		return getOpenCraftingContainer(container).map(ICraftingContainer::getCraftMatrix).orElse(null);
	}

	@Override
	public boolean requiresServerSide() {
		return true;
	}

	private Optional<ICraftingContainer> getOpenCraftingContainer(BackpackContainer container) {
		return container.getOpenContainer().flatMap(c -> (c instanceof ICraftingContainer) ? Optional.of((ICraftingContainer) c) : Optional.empty());
	}

	@Override
	public int getCraftingGridStart(PlayerEntity entityPlayer, BackpackContainer container, int id) {
		return getOpenCraftingContainer(container).map(cc -> {
			List<Slot> recipeSlots = cc.getRecipeSlots();
			if (!recipeSlots.isEmpty()) {
				return recipeSlots.get(0).index;
			}
			return 0;
		}).orElse(0);
	}

	@Override
	public int getCraftingGridSize(PlayerEntity entityPlayer, BackpackContainer container, int id) {
		return getOpenCraftingContainer(container).isPresent() ? 9 : 0;
	}

	@OnlyIn(Dist.CLIENT)
	@Override
	public void initGui(ContainerScreen<BackpackContainer> guiContainer, GuiScreenEvent.InitGuiEvent event) {
		//noop - needs to add buttons earlier than in the event so that upgrade tab can display the buttons on screen open
	}

	@Override
	public boolean isValidContainer(Container container) {
		return container instanceof BackpackContainer;
	}
}
