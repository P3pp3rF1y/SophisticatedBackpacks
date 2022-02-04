package net.p3pp3rf1y.sophisticatedcore.upgrades.xppump;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.entity.player.Player;
import net.p3pp3rf1y.sophisticatedcore.common.gui.UpgradeContainerBase;
import net.p3pp3rf1y.sophisticatedcore.common.gui.UpgradeContainerType;
import net.p3pp3rf1y.sophisticatedcore.util.NBTHelper;

public class XpPumpUpgradeContainer extends UpgradeContainerBase<XpPumpUpgradeWrapper, XpPumpUpgradeContainer> {
	private static final String DATA_LEVEL = "level";
	private static final String DATA_DIRECTION = "direction";
	private static final String DATA_ACTION = "action";
	private static final String ACTION_TAKE_LEVELS = "take";
	private static final String ACTION_STORE_LEVELS_FROM_PLAYER = "store";
	private static final String ACTION_STORE_ALL_PLAYERS_EXPERIENCE = "storeAll";
	private static final String ACTION_TAKE_ALL_LEVELS = "takeAll";
	private static final String DATA_LEVELS_TO_STORE = "levelsToStore";
	private static final String DATA_LEVELS_TO_TAKE = "levelsToTake";
	private static final String DATA_MEND_ITEMS = "mendItems";

	public XpPumpUpgradeContainer(Player player, int upgradeContainerId, XpPumpUpgradeWrapper upgradeWrapper, UpgradeContainerType<XpPumpUpgradeWrapper, XpPumpUpgradeContainer> type) {
		super(player, upgradeContainerId, upgradeWrapper, type);
	}

	public void setDirection(AutomationDirection direction) {
		upgradeWrapper.setDirection(direction);
		sendDataToServer(() -> NBTHelper.putEnumConstant(new CompoundTag(), DATA_DIRECTION, direction));
	}

	public AutomationDirection getDirection() {
		return upgradeWrapper.getDirection();
	}

	public int getLevel() {
		return upgradeWrapper.getLevel();
	}

	public void setLevel(int level) {
		if (level < 0) {
			return;
		}
		upgradeWrapper.setLevel(level);
		sendDataToServer(() -> NBTHelper.putInt(new CompoundTag(), DATA_LEVEL, level));
	}

	public void setLevelsToStore(int levelsToStore) {
		if (levelsToStore < 1) {
			return;
		}

		upgradeWrapper.setLevelsToStore(levelsToStore);
		sendDataToServer(() -> NBTHelper.putInt(new CompoundTag(), DATA_LEVELS_TO_STORE, levelsToStore));
	}

	public void setLevelsToTake(int levelsToTake) {
		if (levelsToTake < 1) {
			return;
		}

		upgradeWrapper.setLevelsToTake(levelsToTake);
		sendDataToServer(() -> NBTHelper.putInt(new CompoundTag(), DATA_LEVELS_TO_TAKE, levelsToTake));
	}

	public void takeLevels() {
		triggerAction(ACTION_TAKE_LEVELS);
	}

	public void storeLevels() {
		triggerAction(ACTION_STORE_LEVELS_FROM_PLAYER);
	}

	public void storeAllExperience() {
		triggerAction(ACTION_STORE_ALL_PLAYERS_EXPERIENCE);
	}

	public void takeAllExperience() {
		triggerAction(ACTION_TAKE_ALL_LEVELS);
	}

	private void triggerAction(String actionName) {
		sendDataToServer(() -> NBTHelper.putString(new CompoundTag(), DATA_ACTION, actionName));
	}

	public int getLevelsToStore() {
		return upgradeWrapper.getLevelsToStore();
	}

	public int getLevelsToTake() {
		return upgradeWrapper.getLevelsToTake();
	}

	public void setMendItems(boolean mendItems) {
		upgradeWrapper.setMendItems(mendItems);
		sendBooleanToServer(DATA_MEND_ITEMS, mendItems);
	}

	public boolean shouldMendItems() {
		return upgradeWrapper.shouldMendItems();
	}

	@Override
	public void handleMessage(CompoundTag data) {
		if (data.contains(DATA_DIRECTION)) {
			setDirection(AutomationDirection.fromName(data.getString(DATA_DIRECTION)));
		} else if (data.contains(DATA_LEVEL)) {
			setLevel(data.getInt(DATA_LEVEL));
		} else if (data.contains(DATA_LEVELS_TO_STORE)) {
			setLevelsToStore(data.getInt(DATA_LEVELS_TO_STORE));
		} else if (data.contains(DATA_LEVELS_TO_TAKE)) {
			setLevelsToTake(data.getInt(DATA_LEVELS_TO_TAKE));
		} else if (data.contains(DATA_MEND_ITEMS)) {
			setMendItems(data.getBoolean(DATA_MEND_ITEMS));
		} else if (data.contains(DATA_ACTION)) {
			switch (data.getString(DATA_ACTION)) {
				case ACTION_TAKE_LEVELS -> upgradeWrapper.giveLevelsToPlayer(player);
				case ACTION_STORE_LEVELS_FROM_PLAYER -> upgradeWrapper.takeLevelsFromPlayer(player);
				case ACTION_TAKE_ALL_LEVELS -> upgradeWrapper.giveAllExperienceToPlayer(player);
				case ACTION_STORE_ALL_PLAYERS_EXPERIENCE -> upgradeWrapper.takeAllExperienceFromPlayer(player);
				default -> {
					//noop
				}
			}
		}
	}
}
