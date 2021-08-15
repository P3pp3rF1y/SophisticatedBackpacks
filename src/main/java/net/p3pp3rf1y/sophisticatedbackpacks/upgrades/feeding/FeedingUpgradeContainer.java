package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.feeding;

import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.nbt.CompoundNBT;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerBase;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerType;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogic;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogicContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;

public class FeedingUpgradeContainer extends UpgradeContainerBase<FeedingUpgradeWrapper, FeedingUpgradeContainer> {
	private static final String DATA_HUNGER_LEVEL = "hungerLevel";
	private static final String DATA_FEED_IMMEDIATELY_WHEN_HURT = "feedImmediatelyWhenHurt";

	private final FilterLogicContainer<FilterLogic> filterLogicContainer;

	public FeedingUpgradeContainer(PlayerEntity player, int containerId, FeedingUpgradeWrapper wrapper, UpgradeContainerType<FeedingUpgradeWrapper, FeedingUpgradeContainer> type) {
		super(player, containerId, wrapper, type);
		filterLogicContainer = new FilterLogicContainer<>(() -> upgradeWrapper.getFilterLogic(), this, slots::add);
	}

	@Override
	public void handleMessage(CompoundNBT data) {
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
		sendDataToServer(() -> NBTHelper.putEnumConstant(new CompoundNBT(), DATA_HUNGER_LEVEL, hungerLevel));
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
