package net.p3pp3rf1y.sophisticatedcore.upgrades.battery;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.InventoryMenu;
import net.minecraftforge.items.IItemHandler;
import net.p3pp3rf1y.sophisticatedcore.SophisticatedCore;
import net.p3pp3rf1y.sophisticatedcore.client.gui.INameableEmptySlot;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.TranslationHelper;
import net.p3pp3rf1y.sophisticatedcore.common.gui.SlotSuppliedHandler;
import net.p3pp3rf1y.sophisticatedcore.common.gui.UpgradeContainerBase;
import net.p3pp3rf1y.sophisticatedcore.common.gui.UpgradeContainerType;
import net.p3pp3rf1y.sophisticatedcore.upgrades.tank.TankUpgradeWrapper;

import java.util.function.Supplier;

public class BatteryUpgradeContainer extends UpgradeContainerBase<BatteryUpgradeWrapper, BatteryUpgradeContainer> {
	public static final ResourceLocation EMPTY_BATTERY_INPUT_SLOT_BACKGROUND = SophisticatedCore.getRL("item/empty_battery_input_slot");
	public static final ResourceLocation EMPTY_BATTERY_OUTPUT_SLOT_BACKGROUND = SophisticatedCore.getRL("item/empty_battery_output_slot");

	public BatteryUpgradeContainer(Player player, int upgradeContainerId, BatteryUpgradeWrapper upgradeWrapper, UpgradeContainerType<BatteryUpgradeWrapper, BatteryUpgradeContainer> type) {
		super(player, upgradeContainerId, upgradeWrapper, type);
		slots.add(new BatteryIOSlot(() -> this.upgradeWrapper.getInventory(), TankUpgradeWrapper.INPUT_SLOT, -100, -100, TranslationHelper.INSTANCE.translUpgradeSlotTooltip("battery_input"))
				.setBackground(InventoryMenu.BLOCK_ATLAS, EMPTY_BATTERY_INPUT_SLOT_BACKGROUND));
		slots.add(new BatteryIOSlot(() -> this.upgradeWrapper.getInventory(), TankUpgradeWrapper.OUTPUT_SLOT, -100, -100, TranslationHelper.INSTANCE.translUpgradeSlotTooltip("battery_output"))
				.setBackground(InventoryMenu.BLOCK_ATLAS, EMPTY_BATTERY_OUTPUT_SLOT_BACKGROUND));
	}

	@Override
	public void handleMessage(CompoundTag data) {
		//noop
	}

	public int getEnergyStored() {
		return upgradeWrapper.getEnergyStored();
	}

	public int getMaxEnergyStored() {
		return upgradeWrapper.getMaxEnergyStored();
	}

	private static class BatteryIOSlot extends SlotSuppliedHandler implements INameableEmptySlot {
		private final Component emptyTooltip;

		public BatteryIOSlot(Supplier<IItemHandler> itemHandlerSupplier, int slot, int xPosition, int yPosition, Component emptyTooltip) {
			super(itemHandlerSupplier, slot, xPosition, yPosition);
			this.emptyTooltip = emptyTooltip;
		}

		@Override
		public boolean hasEmptyTooltip() {
			return true;
		}

		@Override
		public Component getEmptyTooltip() {
			return emptyTooltip;
		}
	}
}
