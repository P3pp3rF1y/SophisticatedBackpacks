package net.p3pp3rf1y.sophisticatedcore.util;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraftforge.common.ForgeConfigSpec;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.common.gui.SortBy;
import net.p3pp3rf1y.sophisticatedcore.inventory.IItemHandlerSimpleInserter;
import net.p3pp3rf1y.sophisticatedcore.inventory.InventoryHandler;
import net.p3pp3rf1y.sophisticatedcore.renderdata.RenderInfo;
import net.p3pp3rf1y.sophisticatedcore.settings.SettingsHandler;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeHandler;
import net.p3pp3rf1y.sophisticatedcore.upgrades.stack.StackUpgradeConfig;

import java.util.Optional;
import java.util.UUID;

@SuppressWarnings("java:S4144") //this is noop wrapper and thus identical implementation isn't an issue especially when it means just returning same field
public class NoopStorageWrapper implements IStorageWrapper {
	public static final NoopStorageWrapper INSTANCE = new NoopStorageWrapper();

	private final UpgradeHandler upgradeHandler = new UpgradeHandler(0, this, new CompoundTag(), () -> {}, () -> {});
	private final InventoryHandler inventoryHandler = new InventoryHandler(0, this, new CompoundTag(), () -> {}, 64, new StackUpgradeConfig(new ForgeConfigSpec.Builder())) {
		@Override
		protected boolean isAllowed(ItemStack stack) {
			return true;
		}
	};
	private final RenderInfo renderInfo = new RenderInfo(() -> () -> {}) {

		@Override
		protected void serializeRenderInfo(CompoundTag renderInfo) {
			//noop
		}

		@Override
		protected Optional<CompoundTag> getRenderInfoTag() {
			return Optional.empty();
		}
	};
	private final SettingsHandler settingsHandler = new SettingsHandler(new CompoundTag(), () -> {}, "", () -> inventoryHandler, () -> renderInfo);

	protected NoopStorageWrapper() {}

	@Override
	public void setSaveHandler(Runnable saveHandler) {
		//noop
	}

	@Override
	public IItemHandlerSimpleInserter getInventoryForUpgradeProcessing() {
		return inventoryHandler;
	}

	@Override
	public InventoryHandler getInventoryHandler() {
		return inventoryHandler;
	}

	@Override
	public IItemHandlerSimpleInserter getInventoryForInputOutput() {
		return inventoryHandler;
	}

	@Override
	public SettingsHandler getSettingsHandler() {
		return settingsHandler;
	}

	@Override
	public UpgradeHandler getUpgradeHandler() {
		return upgradeHandler;
	}

	@Override
	public Optional<UUID> getContentsUuid() {
		return Optional.empty();
	}

	@Override
	public int getMainColor() {
		return -1;
	}

	@Override
	public int getAccentColor() {
		return -1;
	}

	@Override
	public Optional<Integer> getOpenTabId() {
		return Optional.empty();
	}

	@Override
	public void setOpenTabId(int openTabId) {
		//noop
	}

	@Override
	public void removeOpenTabId() {
		//noop
	}

	@Override
	public void setColors(int mainColor, int accentColor) {
		//noop
	}

	@Override
	public void setSortBy(SortBy sortBy) {
		//noop
	}

	@Override
	public SortBy getSortBy() {
		return SortBy.NAME;
	}

	@Override
	public void sort() {
		//noop
	}

	@Override
	public void onContentsNbtUpdated() {
		//noop
	}

	@Override
	public void refreshInventoryForUpgradeProcessing() {
		//noop
	}

	@Override
	public void refreshInventoryForInputOutput() {
		//noop
	}

	@Override
	public void setPersistent(boolean persistent) {
		//noop
	}

	public void setSlotNumbers(int numberOfInventorySlots, int numberOfUpgradeSlots) {
		//noop
	}

	public void setLoot(ResourceLocation lootTableName, float lootPercentage) {
		//noop
	}

	@Override
	public void fillWithLoot(Player playerEntity) {
		//noop
	}

	public void setContentsUuid(UUID storageUuid) {
		//noop
	}

	@Override
	public RenderInfo getRenderInfo() {
		return renderInfo;
	}

	@Override
	public void setColumnsTaken(int columnsTaken) {
		//noop
	}

	@Override
	public int getColumnsTaken() {
		return 0;
	}
}
