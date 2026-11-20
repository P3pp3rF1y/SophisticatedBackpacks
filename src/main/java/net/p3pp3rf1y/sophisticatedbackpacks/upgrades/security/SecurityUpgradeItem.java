package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.security;

import net.minecraft.ChatFormatting;
import net.minecraft.network.chat.Component;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.TooltipFlag;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.SBPTranslationHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModDataComponents;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeItemBase;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeType;

import java.util.List;

public class SecurityUpgradeItem extends UpgradeItemBase<SecurityUpgradeWrapper> {
	public static final UpgradeType<SecurityUpgradeWrapper> TYPE = new UpgradeType<>(SecurityUpgradeWrapper::new);
	private static final List<UpgradeConflictDefinition> UPGRADE_CONFLICT_DEFINITIONS = List
			.of(new UpgradeConflictDefinition(SecurityUpgradeItem.class::isInstance, 0, SBPTranslationHelper.INSTANCE.translError("add.security_exists")));

	public SecurityUpgradeItem() {
		super(Config.SERVER.maxUpgradesPerStorage);
	}

	@Override
	public UpgradeType<SecurityUpgradeWrapper> getType() {
		return TYPE;
	}

	@Override
	public List<UpgradeConflictDefinition> getUpgradeConflicts() {
		return UPGRADE_CONFLICT_DEFINITIONS;
	}

	@Override
	public void appendHoverText(ItemStack stack, Item.TooltipContext context, List<Component> tooltip, TooltipFlag tooltipFlag) {
		super.appendHoverText(stack, context, tooltip, tooltipFlag);
		String ownerName = stack.get(ModDataComponents.SECURITY_OWNER_NAME);
		if (ownerName != null) {
			tooltip.add(Component.translatable("item.sophisticatedbackpacks.security_upgrade.owner", ownerName).withStyle(ChatFormatting.GRAY));
		}
	}
}
