package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.jukebox;

import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.nbt.CompoundNBT;
import net.minecraftforge.items.SlotItemHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContext;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerBase;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerType;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;

public class JukeboxUpgradeContainer extends UpgradeContainerBase<JukeboxUpgradeItem.Wrapper, JukeboxUpgradeContainer> {

	private static final String ACTION_DATA = "action";

	public JukeboxUpgradeContainer(PlayerEntity player, int upgradeContainerId, JukeboxUpgradeItem.Wrapper upgradeWrapper, UpgradeContainerType<JukeboxUpgradeItem.Wrapper, JukeboxUpgradeContainer> type) {
		super(player, upgradeContainerId, upgradeWrapper, type);
		slots.add(new SlotItemHandler(upgradeWrapper.getDiscInventory(), 0, -100, -100) {
			@Override
			public void setChanged() {
				super.setChanged();
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
				if (player.containerMenu instanceof BackpackContainer) {
					BackpackContext context = ((BackpackContainer) player.containerMenu).getBackpackContext();

					if (isBlockBackpack(context)) {
						upgradeWrapper.play(player.level, context.getBackpackPosition(player));
					} else {
						upgradeWrapper.play(player);
					}
				}
			} else if (actionName.equals("stop")) {
				upgradeWrapper.stop(player);
			}
		}
	}

	private boolean isBlockBackpack(BackpackContext context) {
		BackpackContext.ContextType type = context.getType();
		return type == BackpackContext.ContextType.BLOCK_BACKPACK || type == BackpackContext.ContextType.BLOCK_SUB_BACKPACK;
	}

	public void play() {
		sendDataToServer(() -> NBTHelper.putString(new CompoundNBT(), ACTION_DATA, "play"));
	}

	public void stop() {
		sendDataToServer(() -> NBTHelper.putString(new CompoundNBT(), ACTION_DATA, "stop"));
	}
}
