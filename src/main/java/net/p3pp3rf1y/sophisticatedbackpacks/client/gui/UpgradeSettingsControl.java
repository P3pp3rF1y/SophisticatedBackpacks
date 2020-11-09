package net.p3pp3rf1y.sophisticatedbackpacks.client.gui;

import com.mojang.blaze3d.matrix.MatrixStack;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.Rectangle2d;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.CompositeWidget;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerBase;
import org.apache.commons.lang3.mutable.MutableInt;

import java.util.ArrayList;
import java.util.List;

public class UpgradeSettingsControl extends CompositeWidget<UpgradeSettingsTab> {
	private static final int VERTICAL_SPACE = 1;
	private UpgradeSettingsTab openTab = null;

	public UpgradeSettingsControl(int x, int y, BackpackScreen screen) {
		super(x, y);
		setZOffset(-200);
		int i = 0;
		for (UpgradeContainerBase container : screen.getContainer().getUpgradeContainers().values()) {
			addChild(UpgradeSettingsTabManager.getTab(container, x, getTopY(i), screen, tab -> {
						if (differentTabIsOpen(tab)) {
							openTab.changeZOffset(-200);
							openTab.close();
						}
						tab.changeZOffset(200);
						openTab = tab;
					}, tab -> {
						if (openTab != null) {
							openTab.changeZOffset(-200);
							openTab = null;
						}
					}
			));
			i++;
		}
	}

	private boolean differentTabIsOpen(UpgradeSettingsTab<?> tab) {
		return openTab != null && openTab != tab;
	}

	@Override
	protected void renderBg(MatrixStack matrixStack, Minecraft minecraft, int mouseX, int mouseY) {
		//noop
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
