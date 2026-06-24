package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.smithing;

import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiGraphicsExtractor;
import net.minecraft.client.gui.screens.Screen;
import net.minecraft.client.gui.screens.inventory.CyclingSlotBackground;
import net.minecraft.client.gui.screens.inventory.SmithingScreen;
import net.minecraft.client.renderer.entity.layers.HumanoidArmorLayer;
import net.minecraft.client.renderer.entity.state.ArmorStandRenderState;
import net.minecraft.client.renderer.item.ItemModelResolver;
import net.minecraft.core.component.DataComponents;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.EntityTypes;
import net.minecraft.world.entity.EquipmentSlot;
import net.minecraft.world.entity.ItemOwner;
import net.minecraft.world.inventory.Slot;
import net.minecraft.world.item.ItemDisplayContext;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.SmithingTemplateItem;
import net.minecraft.world.item.equipment.Equippable;
import net.minecraft.world.level.Level;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackTranslationHelper;
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
	private final ArmorStandRenderState armorStandPreview = new ArmorStandRenderState();

	public SmithingUpgradeTab(SmithingUpgradeContainer upgradeContainer, Position position, StorageScreenBase<?> screen) {
		super(upgradeContainer, position, screen, BackpackTranslationHelper.INSTANCE.translUpgrade("smithing"),
				BackpackTranslationHelper.INSTANCE.translUpgradeTooltip("smithing"));
		openTabDimension = new Dimension(103, 100);

		armorStandPreview.entityType = EntityTypes.ARMOR_STAND;
		armorStandPreview.showBasePlate = false;
		armorStandPreview.showArms = true;
		armorStandPreview.xRot = 25.0F;
		armorStandPreview.bodyRot = 210.0F;
		updateArmorStandPreview();

		templateIcon = new CyclingSlotBackground(getContainer().getTemplateSlot().index);
		baseIcon = new CyclingSlotBackground(getContainer().getBaseSlot().index);
		additionalIcon = new CyclingSlotBackground(getContainer().getAdditionalSlot().index);

		getContainer().setOnResultChangedHandler(this::updateArmorStandPreview);
	}

	private void updateArmorStandPreview() {
		ItemStack stack = getContainer().getResultSlot().getItem();
		this.armorStandPreview.leftHandItemStack = ItemStack.EMPTY;
		this.armorStandPreview.leftHandItemState.clear();
		this.armorStandPreview.headEquipment = ItemStack.EMPTY;
		this.armorStandPreview.headItem.clear();
		this.armorStandPreview.chestEquipment = ItemStack.EMPTY;
		this.armorStandPreview.legsEquipment = ItemStack.EMPTY;
		this.armorStandPreview.feetEquipment = ItemStack.EMPTY;
		if (!stack.isEmpty()) {
			Equippable equippable = stack.get(DataComponents.EQUIPPABLE);
			ItemModelResolver itemmodelresolver = this.minecraft.getItemModelResolver();
			switch (equippable != null ? equippable.slot() : null) {
				case HEAD :
					if (HumanoidArmorLayer.shouldRender(stack, EquipmentSlot.HEAD)) {
						this.armorStandPreview.headEquipment = stack.copy();
					} else {
						itemmodelresolver.updateForTopItem(this.armorStandPreview.headItem, stack, ItemDisplayContext.HEAD, (Level) null, (ItemOwner) null, 0);
					}
					break;
				case CHEST :
					this.armorStandPreview.chestEquipment = stack.copy();
					break;
				case LEGS :
					this.armorStandPreview.legsEquipment = stack.copy();
					break;
				case FEET :
					this.armorStandPreview.feetEquipment = stack.copy();
					break;
				case null :
				default :
					this.armorStandPreview.leftHandItemStack = stack.copy();
					itemmodelresolver.updateForTopItem(this.armorStandPreview.leftHandItemState, stack, ItemDisplayContext.THIRD_PERSON_LEFT_HAND, (Level) null,
							(ItemOwner) null, 0);
			}
		}
	}

	@Override
	protected void extractBg(GuiGraphicsExtractor guiGraphics, Minecraft minecraft, int mouseX, int mouseY) {
		super.extractBg(guiGraphics, minecraft, mouseX, mouseY);

		if (getContainer().isOpen()) {
			renderSlotBg(guiGraphics, getContainer().getTemplateSlot());
			renderSlotBg(guiGraphics, getContainer().getBaseSlot());
			renderSlotBg(guiGraphics, getContainer().getAdditionalSlot());
			renderSlotBg(guiGraphics, getContainer().getResultSlot());

			templateIcon.extractRenderState(screen.getMenu(), guiGraphics, 0, screen.getLeftX(), screen.getTopY());
			baseIcon.extractRenderState(screen.getMenu(), guiGraphics, 0, screen.getLeftX(), screen.getTopY());
			additionalIcon.extractRenderState(screen.getMenu(), guiGraphics, 0, screen.getLeftX(), screen.getTopY());
		}
	}

	private void renderSlotBg(GuiGraphicsExtractor guiGraphics, Slot slot) {
		GuiHelper.renderSlotsBackground(guiGraphics, slot.x + screen.getGuiLeft() - 1, slot.y + screen.getGuiTop() - 1, 1, 1);
	}

	@Override
	public void extractTooltip(Screen screen, GuiGraphicsExtractor guiGraphics, int mouseX, int mouseY) {
		super.extractTooltip(screen, guiGraphics, mouseX, mouseY);
		renderOnboardingTooltips(guiGraphics, mouseX, mouseY);
	}

	@Override
	protected void extractWidget(GuiGraphicsExtractor guiGraphics, int mouseX, int mouseY, float partialTicks) {
		super.extractWidget(guiGraphics, mouseX, mouseY, partialTicks);

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

		guiGraphics.entity(armorStandPreview, 25f, SmithingScreen.ARMOR_STAND_TRANSLATION, SmithingScreen.ARMOR_STAND_ANGLE, null, getX(),
				getTopY() + 1 + 24 + 16, getX() + getWidth(), getY() + getHeight() - 10);
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
		return mouseX >= slot.x + screen.getGuiLeft() && mouseX < slot.x + screen.getGuiLeft() + 16 && mouseY >= slot.y + screen.getGuiTop()
				&& mouseY < slot.y + screen.getGuiTop() + 16 && slot.getItem().isEmpty();
	}

	private void renderOnboardingTooltips(GuiGraphicsExtractor guiGraphics, int mouseX, int mouseY) {
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

	private void renderOnboardingTooltip(GuiGraphicsExtractor guiGraphics, int mouseX, int mouseY, Component tooltip) {
		guiGraphics.setTooltipForNextFrame(font, font.split(tooltip, 115), mouseX, mouseY);
	}

	private boolean hasRecipeError() {
		return getContainer().getTemplateSlot().hasItem() && getContainer().getBaseSlot().hasItem() && getContainer().getAdditionalSlot().hasItem()
				&& !getContainer().getResultSlot().hasItem();
	}

}
