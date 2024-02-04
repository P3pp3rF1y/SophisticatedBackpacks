package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.anvil;

import com.mojang.blaze3d.systems.RenderSystem;
import com.mojang.blaze3d.vertex.PoseStack;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.GameRenderer;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TranslatableComponent;
import net.minecraft.util.FormattedCharSequence;
import net.minecraft.world.inventory.Slot;
import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.SBPTranslationHelper;
import net.p3pp3rf1y.sophisticatedcore.client.gui.StorageScreenBase;
import net.p3pp3rf1y.sophisticatedcore.client.gui.UpgradeSettingsTab;
import net.p3pp3rf1y.sophisticatedcore.client.gui.controls.TextBox;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.Dimension;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.GuiHelper;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.Position;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.TextureBlitData;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.UV;

import java.util.List;

public class AnvilUpgradeTab extends UpgradeSettingsTab<AnvilUpgradeContainer> {

	public static final TextureBlitData EDIT_ITEM_NAME_BACKGROUND_DISABLED = new TextureBlitData(GuiHelper.GUI_CONTROLS, Dimension.SQUARE_256, new UV(28, 115), new Dimension(100, 16));
	public static final TextureBlitData EDIT_ITEM_NAME_BACKGROUND = new TextureBlitData(GuiHelper.GUI_CONTROLS, Dimension.SQUARE_256, new UV(28, 99), new Dimension(100, 16));
	public static final TextureBlitData PLUS_SIGN = new TextureBlitData(GuiHelper.GUI_CONTROLS, Dimension.SQUARE_256, new UV(113, 203), new Dimension(13, 13));
	public static final TextureBlitData ARROW = new TextureBlitData(GuiHelper.GUI_CONTROLS, Dimension.SQUARE_256, new UV(56, 221), new Dimension(14, 15));
	public static final TextureBlitData RED_CROSS = new TextureBlitData(GuiHelper.GUI_CONTROLS, Dimension.SQUARE_256, new UV(113, 216), new Dimension(15, 15));
	private static final Component TOO_EXPENSIVE_TEXT = new TranslatableComponent("container.repair.expensive");
	private final TextBox itemNameTextBox;
	private ItemStack firstItemCache = ItemStack.EMPTY;

	public AnvilUpgradeTab(AnvilUpgradeContainer upgradeContainer, Position position, StorageScreenBase<?> screen) {
		super(upgradeContainer, position, screen, SBPTranslationHelper.INSTANCE.translUpgrade("anvil"), SBPTranslationHelper.INSTANCE.translUpgradeTooltip("anvil"));
		openTabDimension = new Dimension(103, 92);
		itemNameTextBox = new TextBox(new Position(x + 6, y + 27), new Dimension(84, 13)) {
			@Override
			public boolean mouseClicked(double pMouseX, double pMouseY, int pButton) {
				if (isEditable()) {
					setFocus(true);
					screen.setFocused(itemNameTextBox);
				}
				return super.mouseClicked(pMouseX, pMouseY, pButton);
			}

			@Override
			protected void renderBg(PoseStack poseStack, Minecraft minecraft, int mouseX, int mouseY) {
				super.renderBg(poseStack, minecraft, mouseX, mouseY);
				RenderSystem.setShader(GameRenderer::getPositionTexShader);
				RenderSystem.setShaderColor(1.0F, 1.0F, 1.0F, 1.0F);
				RenderSystem.setShaderTexture(0, GuiHelper.GUI_CONTROLS);

				TextureBlitData textureBlitData = getContainer().getSlots().get(0).hasItem() ? EDIT_ITEM_NAME_BACKGROUND : EDIT_ITEM_NAME_BACKGROUND_DISABLED;

				GuiHelper.blit(poseStack, getX() - 4, getY() - ((getHeight() - 8) / 2) - 1, textureBlitData, getWidth() + 12, getHeight() + 2);
			}
		};
		itemNameTextBox.setTextColor(-1);
		itemNameTextBox.setTextColorUneditable(-1);
		itemNameTextBox.setBordered(false);
		itemNameTextBox.setMaxLength(50);
		itemNameTextBox.setResponder(this::onNameChanged);
		itemNameTextBox.setValue(getInitialNameValue());
		addHideableChild(itemNameTextBox);
		itemNameTextBox.setEditable(!upgradeContainer.getSlots().get(0).getItem().isEmpty());

		getContainer().setSlotsChangeListener(() -> {
			ItemStack firstItem = getContainer().getSlots().get(0).getItem();
			if (!ItemStack.matches(firstItem, firstItemCache) || itemNameTextBox.getValue().isEmpty() != firstItem.isEmpty()) {
				firstItemCache = firstItem;
				itemNameTextBox.setValue(firstItem.isEmpty() ? "" : firstItem.getHoverName().getString());
				itemNameTextBox.setEditable(!firstItem.isEmpty());
			}
		});
	}

	private String getInitialNameValue() {
		ItemStack firstItem = getContainer().getSlots().get(0).getItem();
		String itemName = getContainer().getItemName();
		if (!firstItem.isEmpty() && itemName != null && !itemName.isEmpty()) {
			return itemName;
		}
		return firstItem.isEmpty() ? "" : firstItem.getHoverName().getString();
	}

	private void onNameChanged(String name) {
		if (getContainer().isProcessingOnTakeLogic()) {
			return;
		}
		ItemStack firstItem = getContainer().getSlots().get(0).getItem();
		if (!firstItem.hasCustomHoverName() && name.equals(firstItem.getHoverName().getString())) {
			name = "";
		}
		getContainer().setItemName(name);
	}

	@Override
	protected void renderBg(PoseStack matrixStack, Minecraft minecraft, int mouseX, int mouseY) {
		super.renderBg(matrixStack, minecraft, mouseX, mouseY);
		if (getContainer().isOpen()) {
			renderSlotBg(matrixStack, getContainer().getSlots().get(0));
			renderSlotBg(matrixStack, getContainer().getSlots().get(1));
			renderSlotBg(matrixStack, getContainer().getSlots().get(2));
		}
	}

	private void renderSlotBg(PoseStack poseStack, Slot slot) {
		GuiHelper.renderSlotsBackground(poseStack, slot.x + screen.getGuiLeft() - 1, slot.y + screen.getGuiTop() - 1, 1, 1);
	}

	@Override
	protected void renderWidget(PoseStack poseStack, int mouseX, int mouseY, float partialTicks) {
		super.renderWidget(poseStack, mouseX, mouseY, partialTicks);

		if (!isOpen) {
			return;
		}

		renderCost(poseStack, x + 3, y + 62);

		Slot firstSlot = getContainer().getSlots().get(0);
		int inputSlotsY = firstSlot.y + screen.getGuiTop();
		int firstInputSlotX = firstSlot.x + screen.getGuiLeft();
		int secondInputSlotX = getContainer().getSlots().get(1).x + screen.getGuiLeft();
		Slot resultSlot = getContainer().getSlots().get(2);
		int resultSlotX = resultSlot.x + screen.getGuiLeft();

		GuiHelper.blit(poseStack, firstInputSlotX + 18 + (secondInputSlotX - (firstInputSlotX + 18)) / 2 - PLUS_SIGN.getWidth() / 2 - 1, inputSlotsY + 2, PLUS_SIGN);
		int arrowX = secondInputSlotX + 18 + (resultSlotX - (secondInputSlotX + 18)) / 2 - ARROW.getWidth() / 2 - 1;
		int arrowY = inputSlotsY + 1;
		GuiHelper.blit(poseStack, arrowX, arrowY, ARROW);

		if (firstSlot.hasItem() && !resultSlot.hasItem()) {
			GuiHelper.blit(poseStack, arrowX, arrowY, RED_CROSS);
		}
	}

	@Override
	protected void moveSlotsToTab() {
		Slot firstInputSlot = getContainer().getSlots().get(0);
		firstInputSlot.x = x - screen.getGuiLeft() + 4;
		firstInputSlot.y = y + 42 - screen.getGuiTop() + 1;

		Slot secondInputSlot = getContainer().getSlots().get(1);
		secondInputSlot.x = x - screen.getGuiLeft() + getWidth() / 2 - 9;
		secondInputSlot.y = y + 42 - screen.getGuiTop() + 1;

		Slot resultSlot = getContainer().getSlots().get(2);
		resultSlot.x = x - screen.getGuiLeft() + getWidth() - 2 - 3 - 18;
		resultSlot.y = y + 42 - screen.getGuiTop() + 1;
	}

	protected void renderCost(PoseStack poseStack, int x, int y) {
		RenderSystem.disableBlend();
		int i = getContainer().getCost();
		if (i > 0) {
			int color = 8453920;
			Component component;
			if (i >= 40 && !minecraft.player.getAbilities().instabuild) {
				component = TOO_EXPENSIVE_TEXT;
				color = 16736352;
			} else if (!getContainer().getSlots().get(2).hasItem()) {
				component = null;
			} else {
				component = new TranslatableComponent("container.repair.cost", i);
				if (!getContainer().getSlots().get(2).mayPickup(minecraft.player)) {
					color = 16736352;
				}
			}

			if (component != null) {
				int maxWidth = getWidth() - 9;
				List<FormattedCharSequence> lines = font.split(component, maxWidth);
				fill(poseStack, x, y, x + maxWidth, y + lines.size() * 12, 1325400064);

				int yOffset = 0;
				for (FormattedCharSequence line : lines) {
					int width = font.width(line);
					font.drawShadow(poseStack, line, x + 2 + (float) (maxWidth - width) / 2, y + 2 + yOffset, color);
					yOffset += 12;
				}
			}
		}
	}
}
