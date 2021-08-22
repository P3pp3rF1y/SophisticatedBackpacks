package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.battery;

import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.inventory.container.PlayerContainer;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.text.ITextComponent;
import net.minecraftforge.items.IItemHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.INameableEmptySlot;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.SlotSuppliedHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerBase;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerType;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.tank.TankUpgradeWrapper;

import java.util.function.Supplier;

import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TranslationHelper.translUpgradeSlotTooltip;

public class BatteryUpgradeContainer extends UpgradeContainerBase<BatteryUpgradeWrapper, BatteryUpgradeContainer> {
	public static final ResourceLocation EMPTY_BATTERY_INPUT_SLOT_BACKGROUND = new ResourceLocation(SophisticatedBackpacks.MOD_ID, "item/empty_battery_input_slot");
	public static final ResourceLocation EMPTY_BATTERY_OUTPUT_SLOT_BACKGROUND = new ResourceLocation(SophisticatedBackpacks.MOD_ID, "item/empty_battery_output_slot");

	public BatteryUpgradeContainer(PlayerEntity player, int upgradeContainerId, BatteryUpgradeWrapper upgradeWrapper, UpgradeContainerType<BatteryUpgradeWrapper, BatteryUpgradeContainer> type) {
		super(player, upgradeContainerId, upgradeWrapper, type);
		slots.add(new BatteryIOSlot(() -> this.upgradeWrapper.getInventory(), TankUpgradeWrapper.INPUT_SLOT, -100, -100, translUpgradeSlotTooltip("battery_input"))
				.setBackground(PlayerContainer.BLOCK_ATLAS, EMPTY_BATTERY_INPUT_SLOT_BACKGROUND));
		slots.add(new BatteryIOSlot(() -> this.upgradeWrapper.getInventory(), TankUpgradeWrapper.OUTPUT_SLOT, -100, -100, translUpgradeSlotTooltip("battery_output"))
				.setBackground(PlayerContainer.BLOCK_ATLAS, EMPTY_BATTERY_OUTPUT_SLOT_BACKGROUND));
	}

	@Override
	public void handleMessage(CompoundNBT data) {
		//noop
	}

	public int getEnergyStored() {
		return upgradeWrapper.getEnergyStored();
	}

	public int getMaxEnergyStored() {
		return upgradeWrapper.getMaxEnergyStored();
	}

	private static class BatteryIOSlot extends SlotSuppliedHandler implements INameableEmptySlot {
		private final ITextComponent emptyTooltip;

		public BatteryIOSlot(Supplier<IItemHandler> itemHandlerSupplier, int slot, int xPosition, int yPosition, ITextComponent emptyTooltip) {
			super(itemHandlerSupplier, slot, xPosition, yPosition);
			this.emptyTooltip = emptyTooltip;
		}

		@Override
		public boolean hasEmptyTooltip() {
			return true;
		}

		@Override
		public ITextComponent getEmptyTooltip() {
			return emptyTooltip;
		}
	}
}
