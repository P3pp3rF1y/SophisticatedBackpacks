package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.tank;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.InventoryMenu;
import net.minecraftforge.fluids.FluidStack;
import net.minecraftforge.items.IItemHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.INameableEmptySlot;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.SlotSuppliedHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerBase;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerType;

import java.util.function.Supplier;

import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TranslationHelper.translUpgradeSlotTooltip;

public class TankUpgradeContainer extends UpgradeContainerBase<TankUpgradeWrapper, TankUpgradeContainer> {
	public static final ResourceLocation EMPTY_TANK_INPUT_SLOT_BACKGROUND = new ResourceLocation(SophisticatedBackpacks.MOD_ID, "item/empty_tank_input_slot");
	public static final ResourceLocation EMPTY_TANK_OUTPUT_SLOT_BACKGROUND = new ResourceLocation(SophisticatedBackpacks.MOD_ID, "item/empty_tank_output_slot");

	public TankUpgradeContainer(Player player, int upgradeContainerId, TankUpgradeWrapper upgradeWrapper, UpgradeContainerType<TankUpgradeWrapper, TankUpgradeContainer> type) {
		super(player, upgradeContainerId, upgradeWrapper, type);
		slots.add(new TankIOSlot(() -> this.upgradeWrapper.getInventory(), TankUpgradeWrapper.INPUT_SLOT, -100, -100, translUpgradeSlotTooltip("tank_input"))
				.setBackground(InventoryMenu.BLOCK_ATLAS, EMPTY_TANK_INPUT_SLOT_BACKGROUND));
		slots.add(new TankIOSlot(() -> this.upgradeWrapper.getInventory(), TankUpgradeWrapper.OUTPUT_SLOT, -100, -100, translUpgradeSlotTooltip("tank_output"))
				.setBackground(InventoryMenu.BLOCK_ATLAS, EMPTY_TANK_OUTPUT_SLOT_BACKGROUND));
	}

	@Override
	public void handleMessage(CompoundTag data) {
		//noop
	}

	public FluidStack getContents() {
		return upgradeWrapper.getContents();
	}

	public int getTankCapacity() {
		return upgradeWrapper.getTankCapacity();
	}

	private static class TankIOSlot extends SlotSuppliedHandler implements INameableEmptySlot {
		private final Component emptyTooltip;

		public TankIOSlot(Supplier<IItemHandler> itemHandlerSupplier, int slot, int xPosition, int yPosition, Component emptyTooltip) {
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
