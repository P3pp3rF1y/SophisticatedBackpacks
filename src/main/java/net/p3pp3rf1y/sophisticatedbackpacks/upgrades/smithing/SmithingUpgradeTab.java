package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.smithing;

import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.screens.Screen;
import net.minecraft.client.gui.screens.inventory.CyclingSlotBackground;
import net.minecraft.client.gui.screens.inventory.InventoryScreen;
import net.minecraft.client.gui.screens.inventory.SmithingScreen;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.EquipmentSlot;
import net.minecraft.world.entity.decoration.ArmorStand;
import net.minecraft.world.inventory.Slot;
import net.minecraft.world.item.ArmorItem;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.SmithingTemplateItem;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.SBPTranslationHelper;
import net.p3pp3rf1y.sophisticatedcore.client.gui.StorageScreenBase;
import net.p3pp3rf1y.sophisticatedcore.client.gui.UpgradeSettingsTab;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.*;

import java.util.List;

public class SmithingUpgradeTab extends UpgradeSettingsTab<SmithingUpgradeContainer> {

	public static final TextureBlitData ARROW = new TextureBlitData(GuiHelper.GUI_CONTROLS, Dimension.SQUARE_256, new UV(56, 221), new Dimension(14, 15));
	public static final TextureBlitData RED_CROSS = new TextureBlitData(GuiHelper.GUI_CONTROLS, Dimension.SQUARE_256, new UV(113, 216), new Dimension(15, 15));
	private final CyclingSlotBackground templateIcon;
	private final CyclingSlotBackground baseIcon;
	private final CyclingSlotBackground additionalIcon;
	private final ArmorStand armorStandPreview;

	public SmithingUpgradeTab(SmithingUpgradeContainer upgradeContainer, Position position, StorageScreenBase<?> screen) {
		super(upgradeContainer, position, screen, SBPTranslationHelper.INSTANCE.translUpgrade("smithing"), SBPTranslationHelper.INSTANCE.translUpgradeTooltip("smithing"));
		openTabDimension = new Dimension(103, 100);

		armorStandPreview = new ArmorStand(minecraft.level, 0.0, 0.0, 0.0);
		armorStandPreview.setNoBasePlate(true);
		armorStandPreview.setShowArms(true);
		armorStandPreview.yBodyRot = 210.0F;
		armorStandPreview.setXRot(25.0F);
		armorStandPreview.yHeadRot = armorStandPreview.getYRot();
		armorStandPreview.yHeadRotO = armorStandPreview.getYRot();
		updateArmorStandPreview();

		templateIcon = new CyclingSlotBackground(getContainer().getTemplateSlot().index);
		baseIcon = new CyclingSlotBackground(getContainer().getBaseSlot().index);
		additionalIcon = new CyclingSlotBackground(getContainer().getAdditionalSlot().index);

		getContainer().setOnResultChangedHandler(this::updateArmorStandPreview);
	}

	private void updateArmorStandPreview() {
		ItemStack stack = getContainer().getResultSlot().getItem();
		if (this.armorStandPreview != null) {
			for (EquipmentSlot equipmentslot : EquipmentSlot.values()) {
				armorStandPreview.setItemSlot(equipmentslot, ItemStack.EMPTY);
			}

			if (!stack.isEmpty()) {
				ItemStack itemstack = stack.copy();
				if (stack.getItem() instanceof ArmorItem armoritem) {
					armorStandPreview.setItemSlot(armoritem.getEquipmentSlot(), itemstack);
				} else {
					armorStandPreview.setItemSlot(EquipmentSlot.OFFHAND, itemstack);
				}
			}
		}
	}

	@Override
	protected void renderBg(GuiGraphics guiGraphics, Minecraft minecraft, int mouseX, int mouseY) {
		super.renderBg(guiGraphics, minecraft, mouseX, mouseY);

		if (getContainer().isOpen()) {
			renderSlotBg(guiGraphics, getContainer().getTemplateSlot());
			renderSlotBg(guiGraphics, getContainer().getBaseSlot());
			renderSlotBg(guiGraphics, getContainer().getAdditionalSlot());
			renderSlotBg(guiGraphics, getContainer().getResultSlot());

			templateIcon.render(screen.getMenu(), guiGraphics, 0, screen.getLeftX(), screen.getTopY());
			baseIcon.render(screen.getMenu(), guiGraphics, 0, screen.getLeftX(), screen.getTopY());
			additionalIcon.render(screen.getMenu(), guiGraphics, 0, screen.getLeftX(), screen.getTopY());
		}
	}

	private void renderSlotBg(GuiGraphics guiGraphics, Slot slot) {
		GuiHelper.renderSlotsBackground(guiGraphics, slot.x + screen.getGuiLeft() - 1, slot.y + screen.getGuiTop() - 1, 1, 1);
	}

	@Override
	public void renderTooltip(Screen screen, GuiGraphics guiGraphics, int mouseX, int mouseY) {
		super.renderTooltip(screen, guiGraphics, mouseX, mouseY);
		renderOnboardingTooltips(guiGraphics, mouseX, mouseY);
	}

	@Override
	protected void renderWidget(GuiGraphics guiGraphics, int mouseX, int mouseY, float partialTicks) {
		super.renderWidget(guiGraphics, mouseX, mouseY, partialTicks);

		if (!isOpen) {
			return;
		}

		Slot resultSlot = getContainer().getResultSlot();
		int inputSlotsY = resultSlot.y + screen.getGuiTop();
		int additionalSlotX = getContainer().getAdditionalSlot().x + screen.getGuiLeft();
		int resultSlotX = resultSlot.x + screen.getGuiLeft();

		int arrowX = getArrowX(additionalSlotX, resultSlotX);
		int arrowY = getArrowY(inputSlotsY);
		GuiHelper.blit(guiGraphics, arrowX, arrowY, ARROW);

		if (hasRecipeError()) {
			GuiHelper.blit(guiGraphics, arrowX, arrowY, RED_CROSS);
		}

		InventoryScreen.renderEntityInInventory(guiGraphics, getX() + getWidth() / 2F, getTopY() + getHeight() - 10, 25.0F,
				SmithingScreen.ARMOR_STAND_TRANSLATION, SmithingScreen.ARMOR_STAND_ANGLE, null, armorStandPreview);
	}

	private int getArrowY(int inputSlotsY) {
		return inputSlotsY + 1;
	}

	private int getArrowX(int additionalSlotX, int resultSlotX) {
		return additionalSlotX + 18 + (resultSlotX - (additionalSlotX + 18)) / 2 - ARROW.getWidth() / 2 - 1;
	}

	@Override
	public void tick() {
		super.tick();
		this.templateIcon.tick(SmithingScreen.EMPTY_SLOT_SMITHING_TEMPLATES);
		ItemStack templateItem = getContainer().getTemplateSlot().getItem();
		if (templateItem.getItem() instanceof SmithingTemplateItem smithingTemplate) {
			this.baseIcon.tick(smithingTemplate.getBaseSlotEmptyIcons());
			this.additionalIcon.tick(smithingTemplate.getAdditionalSlotEmptyIcons());
		} else {
			baseIcon.tick(List.of());
			additionalIcon.tick(List.of());
		}
	}

	@Override
	protected void moveSlotsToTab() {
		Slot templateSlot = getContainer().getTemplateSlot();
		templateSlot.x = x - screen.getGuiLeft() + 4;
		templateSlot.y = y - screen.getGuiTop() + 1 + 24;

		Slot baseSlot = getContainer().getBaseSlot();
		baseSlot.x = templateSlot.x + 18;
		baseSlot.y = y - screen.getGuiTop() + 1 + 24;

		Slot additionalSlot = getContainer().getAdditionalSlot();
		additionalSlot.x = baseSlot.x + 18;
		additionalSlot.y = y - screen.getGuiTop() + 1 + 24;

		Slot resultSlot = getContainer().getResultSlot();
		resultSlot.x = x - screen.getGuiLeft() + getWidth() - 2 - 3 - 18;
		resultSlot.y = y - screen.getGuiTop() + 1 + 24;
	}

	private boolean isHoveringRedCross(int mouseX, int mouseY) {
		Slot additionalSlot = getContainer().getAdditionalSlot();
		int arrowX = getArrowX(additionalSlot.x + screen.getGuiLeft(), getContainer().getResultSlot().x + screen.getGuiLeft());
		int arrowY = getArrowY(additionalSlot.y + screen.getGuiTop());
		return mouseX >= arrowX && mouseX < arrowX + RED_CROSS.getWidth() && mouseY >= arrowY && mouseY < arrowY + RED_CROSS.getHeight();
	}

	private boolean isHoveringEmptySlot(Slot slot, int mouseX, int mouseY) {
		return mouseX >= slot.x + screen.getGuiLeft() && mouseX < slot.x + screen.getGuiLeft() + 16 && mouseY >= slot.y + screen.getGuiTop() && mouseY < slot.y + screen.getGuiTop() + 16 && slot.getItem().isEmpty();
	}

	private void renderOnboardingTooltips(GuiGraphics guiGraphics, int mouseX, int mouseY) {
		if (this.hasRecipeError() && isHoveringRedCross(mouseX, mouseY)) {
			Component tooltip = SmithingScreen.ERROR_TOOLTIP;
			renderOnboardingTooltip(guiGraphics, mouseX, mouseY, tooltip);
		} else {
			if (isHoveringEmptySlot(getContainer().getTemplateSlot(), mouseX, mouseY)) {
				renderOnboardingTooltip(guiGraphics, mouseX, mouseY, SmithingScreen.MISSING_TEMPLATE_TOOLTIP);
			} else if (getContainer().getTemplateSlot().getItem().getItem() instanceof SmithingTemplateItem smithingTemplate) {

				if (isHoveringEmptySlot(getContainer().getBaseSlot(), mouseX, mouseY)) {
					renderOnboardingTooltip(guiGraphics, mouseX, mouseY, smithingTemplate.getBaseSlotDescription());
				} else if (isHoveringEmptySlot(getContainer().getAdditionalSlot(), mouseX, mouseY)) {
					renderOnboardingTooltip(guiGraphics, mouseX, mouseY, smithingTemplate.getAdditionSlotDescription());
				}
			}
		}
	}

	private void renderOnboardingTooltip(GuiGraphics guiGraphics, int mouseX, int mouseY, Component tooltip) {
		guiGraphics.pose().pushPose();
		guiGraphics.pose().translate(0, 0, 410);
		guiGraphics.renderTooltip(font, font.split(tooltip, 115), mouseX, mouseY);
		guiGraphics.pose().popPose();
	}

	private boolean hasRecipeError() {
		return getContainer().getTemplateSlot().hasItem() && getContainer().getBaseSlot().hasItem() && getContainer().getAdditionalSlot().hasItem() && !getContainer().getResultSlot().hasItem();
	}

}
