package net.p3pp3rf1y.sophisticatedcore.compat.jei;

import mezz.jei.api.gui.handlers.IGhostIngredientHandler;
import net.minecraft.client.renderer.Rect2i;
import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedcore.SophisticatedCore;
import net.p3pp3rf1y.sophisticatedcore.client.gui.StorageScreenBase;
import net.p3pp3rf1y.sophisticatedcore.common.gui.IFilterSlot;
import net.p3pp3rf1y.sophisticatedcore.common.gui.StorageContainerMenuBase;
import net.p3pp3rf1y.sophisticatedcore.compat.jei.SetGhostSlotMessage;

import java.util.ArrayList;
import java.util.List;

public class StorageGhostIngredientHandler<S extends StorageScreenBase<?>> implements IGhostIngredientHandler<S> {
	@Override
	public <I> List<Target<I>> getTargets(S screen, I i, boolean b) {
		List<Target<I>> targets = new ArrayList<>();
		if (!(i instanceof ItemStack ghostStack)) {
			return targets;
		}
		StorageContainerMenuBase<?> container = screen.getMenu();
		container.getOpenContainer().ifPresent(c -> c.getSlots().forEach(s -> {
			if (s instanceof IFilterSlot && s.mayPlace(ghostStack)) {
				targets.add(new Target<>() {
					@Override
					public Rect2i getArea() {
						return new Rect2i(screen.getGuiLeft() + s.x, screen.getGuiTop() + s.y, 17, 17);
					}

					@Override
					public void accept(I i) {
						SophisticatedCore.PACKET_HANDLER.sendToServer(new SetGhostSlotMessage(ghostStack, s.index));
					}
				});
			}
		}));
		return targets;
	}

	@Override
	public void onComplete() {
		//noop
	}
}
