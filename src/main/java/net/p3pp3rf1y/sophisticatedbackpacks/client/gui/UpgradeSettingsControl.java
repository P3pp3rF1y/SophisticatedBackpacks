package net.p3pp3rf1y.sophisticatedbackpacks.client.gui;

import com.mojang.blaze3d.matrix.MatrixStack;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.Rectangle2d;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.CompositeWidget;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerBase;
import org.apache.commons.lang3.mutable.MutableInt;

import javax.annotation.Nullable;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class UpgradeSettingsControl extends CompositeWidget<UpgradeSettingsTab<?>> {
	private static final int VERTICAL_SPACE = 1;
	@Nullable
	private UpgradeSettingsTab<?> openTab = null;

	public UpgradeSettingsControl(Position position, BackpackScreen screen) {
		super(position);
		int i = 0;
		for (Map.Entry<Integer, UpgradeContainerBase<?, ?>> entry : screen.getContainer().getUpgradeContainers().entrySet()) {
			UpgradeSettingsTab<UpgradeContainerBase<?, ?>> tab = addChild(UpgradeSettingsTabManager.getTab(entry.getValue(), new Position(x, getTopY(i)), screen));
			tab.setHandlers(t -> {
						if (differentTabIsOpen(t)) {
							openTab.close();
						}
						t.setZOffset(200);
						openTab = t;
						screen.getContainer().setOpenTabId(entry.getKey());
					},
					t -> {
						if (openTab != null) {
							openTab.setZOffset(0);
							openTab = null;
							screen.getContainer().removeOpenTabId();
						}
					},
					t -> !differentTabIsOpen(t) || isNotCovered(openTab, t, true),
					t -> openTab == null || isNotCovered(openTab, t, false)
			);
			tab.onAfterInit();
			i++;
		}
	}

	private boolean isNotCovered(UpgradeSettingsTab<?> open, UpgradeSettingsTab<UpgradeContainerBase<?, ?>> t, boolean checkFullyCovered) {
		if (checkFullyCovered) {
			return open.getBottomY() < t.getBottomY() || open.getTopY() > t.getTopY();
		} else {
			return open.getBottomY() < t.getTopY() || open.getTopY() > t.getTopY();
		}
	}

/*
	should show tooltip and should render methods - will be passed in as predicates to the tab
	- if the tab that asks for response is either open or isn't covered in the place of mouse by open tab (or fully for render) these predicates will return true
	then just use it inside of the base tab and that's it
*/

	private boolean differentTabIsOpen(UpgradeSettingsTab<?> tab) {
		return openTab != null && openTab != tab;
	}

	@Override
	protected void renderBg(MatrixStack matrixStack, Minecraft minecraft, int mouseX, int mouseY) {
		//noop
	}

	@Override
	public void afterScreenRender(MatrixStack matrixStack, int mouseX, int mouseY, float partialTicks) {
		children.forEach(tab -> tab.afterScreenRender(matrixStack, mouseX, mouseY, partialTicks));
	}

	private int getTopY(int index) {
		return y + index * (UpgradeSettingsTab.DEFAULT_HEIGHT + VERTICAL_SPACE);
	}

	public int getHeight() {
		MutableInt maxY = new MutableInt(0);

		children.forEach(tab -> {
			int bottomY = tab.getBottomY();
			if (bottomY > maxY.getValue()) {
				maxY.setValue(bottomY);
			}
		});
		return maxY.getValue() - y;
	}

	@Override
	public int getWidth() {
		MutableInt maxWidth = new MutableInt(0);

		children.forEach(tab -> {
			int width = tab.getWidth();
			if (width > maxWidth.getValue()) {
				maxWidth.setValue(width);
			}
		});
		return maxWidth.getValue();
	}

	public List<Rectangle2d> getTabRectangles() {
		List<Rectangle2d> ret = new ArrayList<>();
		children.forEach(child -> ret.add(child.getRectangle()));
		return ret;
	}
}
