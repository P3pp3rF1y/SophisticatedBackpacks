package net.p3pp3rf1y.sophisticatedbackpacks.upgrades;

import net.minecraft.ChatFormatting;
import net.minecraft.network.chat.Component;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.TooltipFlag;
import net.minecraft.world.level.Level;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.ItemBase;

import javax.annotation.Nullable;
import java.util.List;

import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TranslationHelper.getTranslatedLines;
import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TranslationHelper.translItemTooltip;

public abstract class UpgradeItemBase<T extends IUpgradeWrapper> extends ItemBase implements IBackpackUpgradeItem<T> {
	protected UpgradeItemBase() {
		super(new Properties().stacksTo(1));
	}

	@Override
	public void appendHoverText(ItemStack stack, @Nullable Level worldIn, List<Component> tooltip, TooltipFlag flagIn) {
		//noinspection ConstantConditions - item is registered at this point and thus registry name can't be null
		tooltip.addAll(getTranslatedLines(translItemTooltip(stack.getItem().getRegistryName().getPath()), null, ChatFormatting.DARK_GRAY));
	}
}
