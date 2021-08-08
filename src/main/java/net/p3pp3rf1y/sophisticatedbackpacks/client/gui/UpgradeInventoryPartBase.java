package net.p3pp3rf1y.sophisticatedbackpacks.client.gui;

import com.mojang.blaze3d.matrix.MatrixStack;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerBase;

public abstract class UpgradeInventoryPartBase<C extends UpgradeContainerBase<?, ?>> {
	protected final C container;
	protected final int upgradeSlot;

	protected UpgradeInventoryPartBase(int upgradeSlot, C container) {
		this.container = container;
		this.upgradeSlot = upgradeSlot;
	}

	public abstract void render(MatrixStack matrixStack, int mouseX, int mouseY);

	public abstract boolean handleMouseReleased(double mouseX, double mouseY, int button);

	public abstract void renderErrorOverlay(MatrixStack matrixStack);
}
