package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.feeding;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.entity.player.Player;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerBase;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerType;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogic;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogicContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;

public class FeedingUpgradeContainer extends UpgradeContainerBase<FeedingUpgradeWrapper, FeedingUpgradeContainer> {
	private static final String DATA_HUNGER_LEVEL = "hungerLevel";
	private static final String DATA_FEED_IMMEDIATELY_WHEN_HURT = "feedImmediatelyWhenHurt";

	private final FilterLogicContainer<FilterLogic> filterLogicContainer;

	public FeedingUpgradeContainer(Player player, int containerId, FeedingUpgradeWrapper wrapper, UpgradeContainerType<FeedingUpgradeWrapper, FeedingUpgradeContainer> type) {
		super(player, containerId, wrapper, type);
		filterLogicContainer = new FilterLogicContainer<>(() -> upgradeWrapper.getFilterLogic(), this, slots::add);
	}

	@Override
	public void handleMessage(CompoundTag data) {
		if (data.contains(DATA_HUNGER_LEVEL)) {
			setFeedAtHungerLevel(HungerLevel.fromName(data.getString(DATA_HUNGER_LEVEL)));
		} else if (data.contains(DATA_FEED_IMMEDIATELY_WHEN_HURT)) {
			setFeedImmediatelyWhenHurt(data.getBoolean(DATA_FEED_IMMEDIATELY_WHEN_HURT));
		}
		filterLogicContainer.handleMessage(data);
	}

	public FilterLogicContainer<FilterLogic> getFilterLogicContainer() {
		return filterLogicContainer;
	}

	public void setFeedAtHungerLevel(HungerLevel hungerLevel) {
		upgradeWrapper.setFeedAtHungerLevel(hungerLevel);
		sendDataToServer(() -> NBTHelper.putEnumConstant(new CompoundTag(), DATA_HUNGER_LEVEL, hungerLevel));
	}

	public HungerLevel getFeedAtHungerLevel() {
		return upgradeWrapper.getFeedAtHungerLevel();
	}

	public void setFeedImmediatelyWhenHurt(boolean feedImmediatelyWhenHurt) {
		upgradeWrapper.setFeedImmediatelyWhenHurt(feedImmediatelyWhenHurt);
		sendBooleanToServer(DATA_FEED_IMMEDIATELY_WHEN_HURT, feedImmediatelyWhenHurt);
	}

	public boolean shouldFeedImmediatelyWhenHurt() {
		return upgradeWrapper.shouldFeedImmediatelyWhenHurt();
	}
}
