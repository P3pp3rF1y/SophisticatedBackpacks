package net.p3pp3rf1y.sophisticatedbackpacks.common.gui;

import com.google.common.collect.ImmutableMap;
import net.minecraft.core.NonNullList;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.AbstractContainerMenu;
import net.minecraft.world.inventory.ClickType;
import net.minecraft.world.inventory.ContainerListener;
import net.minecraft.world.inventory.Slot;
import net.minecraft.world.item.ItemStack;
import net.minecraftforge.items.IItemHandler;
import net.minecraftforge.items.SlotItemHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackStorage;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackInventoryHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackSettingsHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackBackgroundProperties;
import net.p3pp3rf1y.sophisticatedbackpacks.network.BackpackContentsMessage;
import net.p3pp3rf1y.sophisticatedbackpacks.network.PacketHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.settings.ISettingsCategory;
import net.p3pp3rf1y.sophisticatedbackpacks.settings.SettingsContainerBase;
import net.p3pp3rf1y.sophisticatedbackpacks.settings.backpack.BackpackSettingsCategory;
import net.p3pp3rf1y.sophisticatedbackpacks.settings.backpack.BackpackSettingsContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.settings.memory.MemorySettingsCategory;
import net.p3pp3rf1y.sophisticatedbackpacks.settings.memory.MemorySettingsContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.settings.nosort.NoSortSettingsCategory;
import net.p3pp3rf1y.sophisticatedbackpacks.settings.nosort.NoSortSettingsContainer;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.BiConsumer;

import static net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems.SETTINGS_CONTAINER_TYPE;

public class SettingsContainer extends AbstractContainerMenu implements IContextAwareContainer, ISyncedContainer {
	private static final Map<String, ISettingsContainerFactory<?, ?>> SETTINGS_CONTAINER_FACTORIES;

	static {
		ImmutableMap.Builder<String, ISettingsContainerFactory<?, ?>> builder = new ImmutableMap.Builder<>();
		addFactory(builder, BackpackSettingsCategory.NAME, BackpackSettingsContainer::new);
		addFactory(builder, NoSortSettingsCategory.NAME, NoSortSettingsContainer::new);
		addFactory(builder, MemorySettingsCategory.NAME, MemorySettingsContainer::new);
		SETTINGS_CONTAINER_FACTORIES = builder.build();
	}

	private final Player player;
	private final BackpackContext backpackContext;
	private final IBackpackWrapper backpackWrapper;
	private final BackpackBackgroundProperties backpackBackgroundProperties;
	private final List<Slot> backpackInventorySlots = new ArrayList<>();
	public final NonNullList<ItemStack> ghostItemStacks = NonNullList.create();
	private final Map<String, SettingsContainerBase<?>> settingsContainers = new LinkedHashMap<>();
	public final List<Slot> ghostSlots = new ArrayList<>();
	private CompoundTag lastSettingsNbt = null;

	protected SettingsContainer(int windowId, Player player, BackpackContext backpackContext) {
		super(SETTINGS_CONTAINER_TYPE.get(), windowId);
		this.player = player;
		this.backpackContext = backpackContext;

		backpackWrapper = backpackContext.getBackpackWrapper(player);
		backpackBackgroundProperties = getNumberOfSlots() + backpackWrapper.getColumnsTaken() * backpackWrapper.getNumberOfSlotRows() <= 81 ? BackpackBackgroundProperties.REGULAR : BackpackBackgroundProperties.WIDE;

		addBackpackInventorySlots();
		addSettingsContainers();
	}

	private void addSettingsContainers() {
		BackpackSettingsHandler settingsHandler = backpackWrapper.getSettingsHandler();
		settingsHandler.getSettingsCategories().forEach((name, category) -> settingsContainers.put(name, instantiateContainer(this, name, category)));
	}

	private void addBackpackInventorySlots() {
		BackpackInventoryHandler inventoryHandler = backpackWrapper.getInventoryHandler();

		int slotIndex = 0;
		int yPosition = 18;

		while (slotIndex < inventoryHandler.getSlots()) {
			int lineIndex = slotIndex % getSlotsOnLine();
			int finalSlotIndex = slotIndex;
			backpackInventorySlots.add(addSlot(new ViewOnlyBackpackInventorySlot(inventoryHandler, finalSlotIndex, lineIndex, yPosition)));

			slotIndex++;
			if (slotIndex % getSlotsOnLine() == 0) {
				yPosition += 18;
			}
		}
	}

	@Override
	protected Slot addSlot(Slot slot) {
		slot.index = ghostSlots.size();
		ghostSlots.add(slot);
		ghostItemStacks.add(ItemStack.EMPTY);
		return slot;
	}

	@Override
	public void broadcastChanges() {
		for (int i = 0; i < ghostSlots.size(); ++i) {
			ItemStack itemstack = ghostSlots.get(i).getItem();
			ItemStack itemstack1 = ghostItemStacks.get(i);
			if (!ItemStack.matches(itemstack1, itemstack)) {
				boolean clientStackChanged = !itemstack1.equals(itemstack, true);
				ItemStack itemstack2 = itemstack.copy();
				ghostItemStacks.set(i, itemstack2);

				if (clientStackChanged) {
					for (ContainerListener icontainerlistener : containerListeners) {
						icontainerlistener.slotChanged(this, i, itemstack2);
					}
				}
			}
		}

		if (lastSettingsNbt == null || !lastSettingsNbt.equals(backpackWrapper.getSettingsHandler().getNbt())) {
			lastSettingsNbt = backpackWrapper.getSettingsHandler().getNbt().copy();
			sendBackpackSettingsToClient();
		}
	}

	public void detectSettingsChangeAndReload() {
		if (player.level.isClientSide) {
			backpackWrapper.getContentsUuid().ifPresent(uuid -> {
				BackpackStorage storage = BackpackStorage.get();
				if (storage.removeUpdatedBackpackSettingsFlag(uuid)) {
					backpackWrapper.getSettingsHandler().reloadFrom(storage.getOrCreateBackpackContents(uuid));
				}
			});
		}
	}

	private void sendBackpackSettingsToClient() {
		if (player.level.isClientSide) {
			return;
		}

		backpackWrapper.getContentsUuid().ifPresent(uuid -> {
			CompoundTag settingsContents = new CompoundTag();
			CompoundTag settingsNbt = backpackWrapper.getSettingsHandler().getNbt();
			if (!settingsNbt.isEmpty()) {
				settingsContents.put(BackpackSettingsHandler.SETTINGS_TAG, settingsNbt);
				PacketHandler.sendToClient((ServerPlayer) player, new BackpackContentsMessage(uuid, settingsContents));
			}
		});
	}

	@Override
	public void addSlotListener(ContainerListener listener) {
		if (listener instanceof ServerPlayer serverPlayer && backpackWrapper.getInventoryHandler().getStackSizeMultiplier() > 1) {
			super.setSynchronizer(new HighStackCountSynchronizer(serverPlayer));
			return;
		}
		super.addSlotListener(listener);
	}

	@Override
	public Slot getSlot(int slotId) {
		return ghostSlots.get(slotId);
	}

	public Optional<ItemStack> getMemorizedStackInSlot(int slotId) {
		return backpackWrapper.getSettingsHandler().getTypeCategory(MemorySettingsCategory.class).getSlotFilterItem(slotId).map(ItemStack::new);
	}

	public int getSlotsOnLine() {
		return backpackBackgroundProperties.getSlotsOnLine() - backpackWrapper.getColumnsTaken();
	}

	public int getNumberOfSlots() {
		return backpackWrapper.getInventoryHandler().getSlots();
	}

	@Override
	public boolean stillValid(Player player) {
		return true;
	}

	@Override
	public BackpackContext getBackpackContext() {
		return backpackContext;
	}

	public List<Slot> getBackpackInventorySlots() {
		return backpackInventorySlots;
	}

	@Override
	public void handleMessage(CompoundTag data) {
		if (data.contains("categoryName")) {
			String categoryName = data.getString("categoryName");
			if (settingsContainers.containsKey(categoryName)) {
				settingsContainers.get(categoryName).handleMessage(data);
			}
		}
	}

	@Override
	public void clicked(int slotId, int dragType, ClickType clickTypeIn, Player player) {
		//noop
	}

	public BackpackBackgroundProperties getBackpackBackgroundProperties() {
		return backpackBackgroundProperties;
	}

	public static SettingsContainer fromBuffer(int windowId, Inventory playerInventory, FriendlyByteBuf packetBuffer) {
		return new SettingsContainer(windowId, playerInventory.player, BackpackContext.fromBuffer(packetBuffer));
	}

	public void forEachSettingsContainer(BiConsumer<String, ? super SettingsContainerBase<?>> consumer) {
		settingsContainers.forEach(consumer);
	}

	public Player getPlayer() {
		return player;
	}

	private static class ViewOnlyBackpackInventorySlot extends SlotItemHandler {
		public ViewOnlyBackpackInventorySlot(IItemHandler inventoryHandler, int slotIndex, int lineIndex, int yPosition) {
			super(inventoryHandler, slotIndex, 8 + lineIndex * 18, yPosition);
		}

		@Override
		public boolean mayPickup(Player playerIn) {
			return false;
		}
	}

	public int getNumberOfRows() {
		return backpackWrapper.getNumberOfSlotRows();
	}

	private static <C extends ISettingsCategory, T extends SettingsContainerBase<C>> void addFactory(
			ImmutableMap.Builder<String, ISettingsContainerFactory<?, ?>> builder, String categoryName, ISettingsContainerFactory<C, T> factory) {
		builder.put(categoryName, factory);
	}

	public interface ISettingsContainerFactory<C extends ISettingsCategory, T extends SettingsContainerBase<C>> {
		T create(SettingsContainer settingsContainer, String categoryName, C category);
	}

	private static <C extends ISettingsCategory> SettingsContainerBase<C> instantiateContainer(SettingsContainer settingsContainer, String name, C category) {
		//noinspection unchecked
		return (SettingsContainerBase<C>) getSettingsContainerFactory(name).create(settingsContainer, name, category);
	}

	private static <C extends ISettingsCategory, T extends SettingsContainerBase<C>> ISettingsContainerFactory<C, T> getSettingsContainerFactory(String name) {
		//noinspection unchecked
		return (ISettingsContainerFactory<C, T>) SETTINGS_CONTAINER_FACTORIES.get(name);
	}
}
