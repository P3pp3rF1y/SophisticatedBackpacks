package net.p3pp3rf1y.sophisticatedbackpacks.compat.quark;

import net.minecraft.client.gui.widget.button.Button;
import net.minecraft.inventory.container.Slot;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.fml.DistExecutor;
import net.minecraftforge.fml.common.ObfuscationReflectionHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.compat.ICompat;
import vazkii.quark.base.module.ModuleLoader;
import vazkii.quark.content.management.client.gui.MiniInventoryButton;
import vazkii.quark.content.management.module.InventorySortingModule;

public class QuarkCompat implements ICompat {
	@Override
	public void setup() {
		DistExecutor.unsafeRunWhenOn(Dist.CLIENT, () -> () -> {
			if (ModuleLoader.INSTANCE.isModuleEnabled(InventorySortingModule.class)) {
				BackpackScreen.setButtonReplacer(new BackpackScreen.IButtonReplacer() {
					@Override
					public boolean shouldReplace(BackpackScreen screen, Button button) {
						if (!(button instanceof MiniInventoryButton)) {
							return false;
						}
						try {
							//this logic isn't ideal by far, but best that can be used to identify the actual sort backpack button
							MiniInventoryButton miniInventoryButton = ((MiniInventoryButton) button);
							int type = ObfuscationReflectionHelper.getPrivateValue(MiniInventoryButton.class, miniInventoryButton, "type");
							Slot slot = screen.getMenu().getSlot(8);
							int x = screen.getGuiLeft() + slot.x + 6;
							int y = screen.getGuiTop() + slot.y - 13;
							return type == 0 && button.x == x && button.y == y;
						}
						catch (ObfuscationReflectionHelper.UnableToFindFieldException | ObfuscationReflectionHelper.UnableToAccessFieldException | NullPointerException ex) {
							SophisticatedBackpacks.LOGGER.error("Error checking for Quark Sort button ", ex);
							return false;
						}
					}

					@Override
					public Button replace(BackpackScreen screen, Button button) {
						return new MiniInventoryButton(screen, 0, button.x - screen.getGuiLeft(), button.y - screen.getGuiTop(), "quark.gui.button.sort_container", b -> screen.getMenu().sort());
					}
				});
			}
		});
	}
}
