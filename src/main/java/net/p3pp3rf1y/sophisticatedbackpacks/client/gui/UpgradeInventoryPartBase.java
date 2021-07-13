package net.p3pp3rf1y.sophisticatedbackpacks.client.gui;

import com.mojang.blaze3d.matrix.MatrixStack;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerBase;

public abstract class UpgradeInventoryPartBase<C extends UpgradeContainerBase<?, ?>> {
	protected final C container;

	public UpgradeInventoryPartBase(C container) {
		this.container = container;
	}

	public abstract void render(MatrixStack matrixStack, int mouseX, int mouseY);
}
