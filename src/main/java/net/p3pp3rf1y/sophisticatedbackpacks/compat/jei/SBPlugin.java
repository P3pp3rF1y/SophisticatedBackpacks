package net.p3pp3rf1y.sophisticatedbackpacks.compat.jei;

import mezz.jei.api.IModPlugin;
import mezz.jei.api.JeiPlugin;
import mezz.jei.api.gui.handlers.IGuiContainerHandler;
import mezz.jei.api.registration.IGuiHandlerRegistration;
import net.minecraft.client.renderer.Rectangle2d;
import net.minecraft.util.ResourceLocation;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackScreen;

import java.util.Collections;
import java.util.List;

@SuppressWarnings("unused")
@JeiPlugin
public class SBPlugin implements IModPlugin {
	@Override
	public ResourceLocation getPluginUid() {
		return new ResourceLocation(SophisticatedBackpacks.MOD_ID, "default");
	}

	@Override
	public void registerGuiHandlers(IGuiHandlerRegistration registration) {
		registration.addGuiContainerHandler(BackpackScreen.class, new IGuiContainerHandler<BackpackScreen>() {
			@Override
			public List<Rectangle2d> getGuiExtraAreas(BackpackScreen gui) {
				return Collections.singletonList(new Rectangle2d(gui.getGuiLeft() - gui.getUpgradesOffset(), gui.getGuiTop() + gui.getUpgradeTop(), 32, gui.getUpgradeHeight()));
			}
		});
	}
}
