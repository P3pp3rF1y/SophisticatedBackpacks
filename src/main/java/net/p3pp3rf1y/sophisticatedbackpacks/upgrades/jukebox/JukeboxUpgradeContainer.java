package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.jukebox;

import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.inventory.container.ContainerType;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.util.math.BlockPos;
import net.minecraftforge.items.SlotItemHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerBase;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerType;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;

import static net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems.BACKPACK_BLOCK_CONTAINER_TYPE;
import static net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems.BLOCK_SUBBACKPACK_CONTAINER_TYPE;

public class JukeboxUpgradeContainer extends UpgradeContainerBase<JukeboxUpgradeItem.Wrapper, JukeboxUpgradeContainer> {

	private static final String ACTION_DATA = "action";

	public JukeboxUpgradeContainer(PlayerEntity player, int upgradeContainerId, JukeboxUpgradeItem.Wrapper upgradeWrapper, UpgradeContainerType<JukeboxUpgradeItem.Wrapper, JukeboxUpgradeContainer> type) {
		super(player, upgradeContainerId, upgradeWrapper, type);
		slots.add(new SlotItemHandler(upgradeWrapper.getDiscInventory(), 0, -100, -100) {
			@Override
			public void onSlotChanged() {
				super.onSlotChanged();
				if (upgradeWrapper.isPlaying()) {
					upgradeWrapper.stop(player);
				}
			}
		});
	}

	@Override
	public void handleMessage(CompoundNBT data) {
		if (data.contains(ACTION_DATA)) {
			String actionName = data.getString(ACTION_DATA);
			if (actionName.equals("play")) {
				if (isBlockBackpack(player.openContainer.getType())) {
					upgradeWrapper.play(player.world, getBlockBackpackPosition());
				} else {
					upgradeWrapper.play(player);
				}
			} else if (actionName.equals("stop")) {
				upgradeWrapper.stop(player);
			}
		}
	}

	private boolean isBlockBackpack(ContainerType<?> type) {
		return type == BLOCK_SUBBACKPACK_CONTAINER_TYPE.get() || type == BACKPACK_BLOCK_CONTAINER_TYPE.get();
	}

	private BlockPos getBlockBackpackPosition() {
		return player.openContainer instanceof BackpackContainer ? ((BackpackContainer) player.openContainer).getBackpackContext().getBackpackPosition(player) : BlockPos.ZERO;
	}

	public void play() {
		sendDataToServer(() -> NBTHelper.putString(new CompoundNBT(), ACTION_DATA, "play"));
	}

	public void stop() {
		sendDataToServer(() -> NBTHelper.putString(new CompoundNBT(), ACTION_DATA, "stop"));
	}
}
