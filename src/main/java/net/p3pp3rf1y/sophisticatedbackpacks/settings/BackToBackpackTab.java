package net.p3pp3rf1y.sophisticatedbackpacks.settings;

import net.minecraft.network.chat.Component;
import net.neoforged.neoforge.network.PacketDistributor;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackTranslationHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.network.BackpackOpenPayload;
import net.p3pp3rf1y.sophisticatedcore.client.gui.Tab;
import net.p3pp3rf1y.sophisticatedcore.client.gui.controls.ImageButton;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.*;

public class BackToBackpackTab extends Tab {
	private static final TextureBlitData ICON = new TextureBlitData(GuiHelper.ICONS, Dimension.SQUARE_256, new UV(64, 80), Dimension.SQUARE_16);

	protected BackToBackpackTab(Position position) {
		super(position, Component.translatable(BackpackTranslationHelper.INSTANCE.translGui("back_to_backpack.tooltip")),
				onTabIconClicked -> new ImageButton(new Position(position.x() + 1, position.y() + 4), Dimension.SQUARE_16, ICON, onTabIconClicked));
	}

	@Override
	protected void onTabIconClicked(int button) {
		PacketDistributor.sendToServer(new BackpackOpenPayload());
	}
}
