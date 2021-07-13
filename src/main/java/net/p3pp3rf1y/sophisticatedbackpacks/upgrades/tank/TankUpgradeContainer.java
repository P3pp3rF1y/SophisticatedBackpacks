package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.tank;

import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.inventory.container.PlayerContainer;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.util.ResourceLocation;
import net.minecraftforge.fluids.FluidStack;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.SlotSuppliedHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerBase;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerType;

public class TankUpgradeContainer extends UpgradeContainerBase<TankUpgradeWrapper, TankUpgradeContainer> {
	public static final ResourceLocation EMPTY_TANK_INPUT_SLOT_BACKGROUND = new ResourceLocation(SophisticatedBackpacks.MOD_ID, "item/empty_tank_input_slot");
	public static final ResourceLocation EMPTY_TANK_OUTPUT_SLOT_BACKGROUND = new ResourceLocation(SophisticatedBackpacks.MOD_ID, "item/empty_tank_output_slot");

	public TankUpgradeContainer(PlayerEntity player, int upgradeContainerId, TankUpgradeWrapper upgradeWrapper, UpgradeContainerType<TankUpgradeWrapper, TankUpgradeContainer> type) {
		super(player, upgradeContainerId, upgradeWrapper, type);
		slots.add(new SlotSuppliedHandler(() -> this.upgradeWrapper.getInventory(), TankUpgradeWrapper.INPUT_SLOT, -100, -100)
				.setBackground(PlayerContainer.LOCATION_BLOCKS_TEXTURE, EMPTY_TANK_INPUT_SLOT_BACKGROUND));
		slots.add(new SlotSuppliedHandler(() -> this.upgradeWrapper.getInventory(), TankUpgradeWrapper.OUTPUT_SLOT, -100, -100)
				.setBackground(PlayerContainer.LOCATION_BLOCKS_TEXTURE, EMPTY_TANK_OUTPUT_SLOT_BACKGROUND));
	}

	@Override
	public void handleMessage(CompoundNBT data) {
		//noop
	}

	public FluidStack getContents() {
		return upgradeWrapper.getContents();
	}

	public int getTankCapacity() {
		return upgradeWrapper.getTankCapacity();
	}
}
