package net.p3pp3rf1y.sophisticatedbackpacks.client.gui;

import com.google.common.collect.ImmutableList;
import com.mojang.blaze3d.matrix.MatrixStack;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.Rectangle2d;
import net.minecraft.item.ItemStack;
import net.minecraft.util.IReorderingProcessor;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.text.ITextComponent;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.CompositeWidget;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ItemButton;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.Label;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.Widget;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerBase;

import java.util.ArrayList;
import java.util.List;

public abstract class UpgradeSettingsTab<C extends UpgradeContainerBase> extends CompositeWidget<Widget> {
	protected static final ResourceLocation UPGRADE_CONTROLS = new ResourceLocation(SophisticatedBackpacks.MOD_ID, "textures/gui/upgrade_controls.png");
	private static final int TEXTURE_WIDTH = 256;
	private static final int TEXTURE_HEIGHT = 256;
	public static final int DEFAULT_HEIGHT = 24;
	private static final int DEFAULT_WIDTH = 21;
	private final List<IReorderingProcessor> closedTooltip;
	private int width = DEFAULT_WIDTH;
	private int height = DEFAULT_HEIGHT;
	private boolean isOpen = false;
	private final C upgradeContainer;
	private final TabDimensions openTabDimensions;
	private final List<Widget> hideableChildren = new ArrayList<>();

	public UpgradeSettingsTab(C upgradeContainer, int x, int y, TabDimensions openTabDimensions, ItemStack iconStack, ITextComponent tabLabel, ITextComponent closedTooltip) {
		super(x, y);
		this.upgradeContainer = upgradeContainer;
		this.openTabDimensions = openTabDimensions;
		this.closedTooltip = ImmutableList.of(closedTooltip.func_241878_f());
		addChild(new ItemButton(x + 1, y + 4, this::onTabIconClicked, iconStack) {
			@Override
			protected void renderWidget(MatrixStack matrixStack, int mouseX, int mouseY, float partialTicks) {
				super.renderWidget(matrixStack, mouseX, mouseY, partialTicks);
				if (!isOpen && isMouseOver(mouseX, mouseY)) {
					GuiHelper.setTooltipToRender(UpgradeSettingsTab.this.closedTooltip);
				}
			}
		});
		addHideableChild(new Label(x + 20, y + 8, tabLabel));
	}

	private boolean onTabIconClicked(int button) {
		if (button != 0) {
			return false;
		}
		setOpen(!isOpen);
		return true;
	}

	protected C getContainer() {
		return upgradeContainer;
	}

	protected <U extends Widget> void addHideableChild(U widget) {
		hideableChildren.add(widget);
	}

	@Override
	protected void renderBg(MatrixStack matrixStack, Minecraft minecraft, int mouseX, int mouseY) {
		minecraft.getTextureManager().bindTexture(UPGRADE_CONTROLS);

		int halfHeight = height / 2;
		blit(matrixStack, x, y, (float) TEXTURE_WIDTH - width, 0, width, halfHeight, TEXTURE_WIDTH, TEXTURE_HEIGHT);
		blit(matrixStack, x, y + halfHeight, (float) TEXTURE_WIDTH - width, (float) TEXTURE_HEIGHT - halfHeight, width, halfHeight, TEXTURE_WIDTH, TEXTURE_HEIGHT);
		blit(matrixStack, x - 3, y, TEXTURE_WIDTH / 2, TEXTURE_HEIGHT - height, 3, height);
	}

	@Override
	public int getWidth() {
		return width;
	}

	@Override
	public int getHeight() {
		return height;
	}

	public int getBottomY() {
		return y + getHeight();
	}

	public void close() {
		setOpen(false);
		onTabClose();
	}

	protected void onTabOpen() {
		width = openTabDimensions.getWidth();
		height = openTabDimensions.getHeight();

		hideableChildren.forEach(this::addChild);
	}

	protected void onTabClose() {
		width = DEFAULT_WIDTH;
		height = DEFAULT_HEIGHT;

		children.removeAll(hideableChildren);
	}

	private void setOpen(boolean isOpen) {
		this.isOpen = isOpen;
		if (isOpen) {
			onTabOpen();
		} else {
			onTabClose();
		}
	}

	public boolean isOpen() {
		return isOpen;
	}

	public Rectangle2d getRectangle() {
		return new Rectangle2d(x, y, width, height);
	}

	public static class TabDimensions {
		private final int width;
		private final int height;

		public TabDimensions(int width, int height) {
			this.width = width;
			this.height = height;
		}

		public int getWidth() {
			return width;
		}

		public int getHeight() {
			return height;
		}
	}
}
