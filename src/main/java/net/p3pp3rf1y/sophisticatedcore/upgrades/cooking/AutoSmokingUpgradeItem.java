package net.p3pp3rf1y.sophisticatedcore.upgrades.cooking;

import net.minecraft.world.item.CreativeModeTab;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeItemBase;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeType;

public class AutoSmokingUpgradeItem extends UpgradeItemBase<AutoCookingUpgradeWrapper.AutoSmokingUpgradeWrapper> implements IAutoCookingUpgradeItem {
	public static final UpgradeType<AutoCookingUpgradeWrapper.AutoSmokingUpgradeWrapper> TYPE = new UpgradeType<>(AutoCookingUpgradeWrapper.AutoSmokingUpgradeWrapper::new);
	private final AutoCookingUpgradeConfig autoSmokingUpgradeConfig;

	public AutoSmokingUpgradeItem(CreativeModeTab itemGroup, AutoCookingUpgradeConfig autoSmokingUpgradeConfig) {super(itemGroup);
		this.autoSmokingUpgradeConfig = autoSmokingUpgradeConfig;
	}

	@Override
	public UpgradeType<AutoCookingUpgradeWrapper.AutoSmokingUpgradeWrapper> getType() {
		return TYPE;
	}

	@Override
	public AutoCookingUpgradeConfig getAutoCookingUpgradeConfig() {
		return autoSmokingUpgradeConfig;
	}
}
