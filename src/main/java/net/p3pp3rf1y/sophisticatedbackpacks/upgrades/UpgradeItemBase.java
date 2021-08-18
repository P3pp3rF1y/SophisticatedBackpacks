package net.p3pp3rf1y.sophisticatedbackpacks.upgrades;

import net.minecraft.client.util.ITooltipFlag;
import net.minecraft.item.ItemStack;
import net.minecraft.util.text.ITextComponent;
import net.minecraft.util.text.TextFormatting;
import net.minecraft.world.World;
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
	public void appendHoverText(ItemStack stack, @Nullable World worldIn, List<ITextComponent> tooltip, ITooltipFlag flagIn) {
		//noinspection ConstantConditions - item is registered at this point and thus registry name can't be null
		tooltip.addAll(getTranslatedLines(translItemTooltip(stack.getItem().getRegistryName().getPath()), null, TextFormatting.DARK_GRAY));
	}
}
