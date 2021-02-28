package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.crafting;

import net.minecraft.inventory.container.Slot;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackScreen;

import java.util.List;

@OnlyIn(Dist.CLIENT)
public interface ICraftingUIPart {
	void onCraftingSlotsDisplayed(List<Slot> slots);

	void onCraftingSlotsHidden();

	int getWidth();

	void setBackpackScreen(BackpackScreen screen);

	ICraftingUIPart NOOP = new ICraftingUIPart() {
		@Override
		public void onCraftingSlotsDisplayed(List<Slot> slots) {
			//noop
		}

		@Override
		public void onCraftingSlotsHidden() {
			//noop
		}

		@Override
		public int getWidth() {
			return 0;
		}

		@Override
		public void setBackpackScreen(BackpackScreen screen) {
			//noop
		}
	};
}
