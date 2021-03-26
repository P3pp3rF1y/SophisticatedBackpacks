package net.p3pp3rf1y.sophisticatedbackpacks.compat.quark;

import net.minecraft.entity.player.PlayerInventory;
import net.minecraft.util.text.ITextComponent;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;
import vazkii.quark.api.IQuarkButtonIgnored;

public class QuarkReadyBackpackScreen extends BackpackScreen implements IQuarkButtonIgnored {
	public QuarkReadyBackpackScreen(BackpackContainer screenContainer, PlayerInventory inv, ITextComponent title) {
		super(screenContainer, inv, title);
	}
}
