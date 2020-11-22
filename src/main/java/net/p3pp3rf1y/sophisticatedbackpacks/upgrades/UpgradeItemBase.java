package net.p3pp3rf1y.sophisticatedbackpacks.upgrades;

import net.minecraft.client.util.ITooltipFlag;
import net.minecraft.item.ItemStack;
import net.minecraft.util.text.ITextComponent;
import net.minecraft.util.text.TextFormatting;
import net.minecraft.world.World;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.items.ItemBase;

import javax.annotation.Nullable;
import java.util.List;

import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.TranslationHelper.getTranslatedLines;
import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.TranslationHelper.translUpgradeItemTooltip;

public abstract class UpgradeItemBase<T extends IUpgradeWrapper> extends ItemBase implements IBackpackUpgradeItem<T> {
	protected UpgradeItemBase() {
		super(new Properties().maxStackSize(1));
	}

	@OnlyIn(Dist.CLIENT)
	@Override
	public void addInformation(ItemStack stack, @Nullable World worldIn, List<ITextComponent> tooltip, ITooltipFlag flagIn) {
		//noinspection ConstantConditions - item is registered at this point and thus registry name can't be null
		tooltip.addAll(getTranslatedLines(translUpgradeItemTooltip(stack.getItem().getRegistryName().getPath()), null, TextFormatting.DARK_GRAY));
	}
}
