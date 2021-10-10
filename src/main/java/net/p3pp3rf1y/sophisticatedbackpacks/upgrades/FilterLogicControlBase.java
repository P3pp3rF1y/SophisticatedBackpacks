package net.p3pp3rf1y.sophisticatedbackpacks.upgrades;

import com.mojang.blaze3d.matrix.MatrixStack;
import net.minecraft.client.Minecraft;
import net.minecraft.inventory.container.Slot;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.text.ITextProperties;
import net.minecraft.util.text.StringTextComponent;
import net.minecraft.util.text.TextFormatting;
import net.minecraft.util.text.TranslationTextComponent;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.BackpackWidget;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.Button;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ButtonDefinition;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ButtonDefinitions;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.CompositeBackpackWidget;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ToggleButton;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.GuiHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Position;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TranslationHelper;

import javax.annotation.Nullable;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.function.DoubleConsumer;
import java.util.function.IntConsumer;

import static net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogicControlBase.MatchButton.*;

public abstract class FilterLogicControlBase<F extends FilterLogicBase, S extends Slot, C extends FilterLogicContainerBase<F, S>>
		extends CompositeBackpackWidget<BackpackWidget> {
	public static final int TAG_FONT_COLOR = 16383998;
	public static final int MORE_TAGS_FONT_COLOR = 13882323;
	private static final int TEXTURE_WIDTH = 256;
	private static final int TEXTURE_HEIGHT = 256;
	private static final int MAX_TAG_NAME_WIDTH = 68;

	protected final MatchButton[] showMatchButtons;
	protected final int slotsTopYOffset;
	protected final int slotsPerRow;
	protected final int slotsInExtraRow;
	protected final int fullSlotRows;
	private final int totalSlotRows;
	private final BackpackScreen screen;
	protected final C container;
	private final int height;
	private final int width;
	private final List<ITextProperties> addTagTooltip = new ArrayList<>();
	private final List<ITextProperties> removeTagTooltip = new ArrayList<>();
	private final List<ITextProperties> tagListTooltip = new ArrayList<>();
	@Nullable
	private ToggleButton<Boolean> nbtButton = null;
	@Nullable
	private ToggleButton<Boolean> durabilityButton = null;
	private int tagButtonsYOffset;

	protected FilterLogicControlBase(BackpackScreen screen, C container, Position position, boolean buttonsVisible, int slotsPerRow, MatchButton... showMatchButtons) {
		super(position);
		this.screen = screen;
		this.container = container;
		slotsTopYOffset = buttonsVisible ? 21 : 0;
		this.slotsPerRow = slotsPerRow;
		this.showMatchButtons = showMatchButtons;
		fullSlotRows = container.getFilterSlots().size() / slotsPerRow;
		slotsInExtraRow = container.getFilterSlots().size() % slotsPerRow;
		totalSlotRows = fullSlotRows + (slotsInExtraRow > 0 ? 1 : 0);

		if (shouldShow(ALLOW_LIST)) {
			addChild(new ToggleButton<>(new Position(x, y), ButtonDefinitions.ALLOW_LIST, button -> container.setAllowList(!container.isAllowList()), container::isAllowList));
		}
		if (shouldShow(PRIMARY_MATCH)) {
			addChild(new ToggleButton<>(new Position(x + 18, y), ButtonDefinitions.PRIMARY_MATCH,
					button -> {
						PrimaryMatch next = container.getPrimaryMatch().next();
						if (next == PrimaryMatch.TAGS) {
							container.getFilterSlots().forEach(slot -> slot.x = BackpackScreen.DISABLED_SLOT_X_POS);
							onTagsMatchSelected();
						}
						container.setPrimaryMatch(next);
						setDurabilityAndNbtButtonsVisibility();
						moveSlotsToView();
					}, container::getPrimaryMatch));
			addTagButtons();
		}
		if (shouldShow(DURABILITY)) {
			durabilityButton = new ToggleButton<>(new Position(x + 36, y), ButtonDefinitions.MATCH_DURABILITY,
					button -> container.setMatchDurability(!container.shouldMatchDurability()), container::shouldMatchDurability);
			addChild(durabilityButton);
		}
		if (shouldShow(NBT)) {
			nbtButton = new ToggleButton<>(new Position(x + 54, y), ButtonDefinitions.MATCH_NBT,
					button -> container.setMatchNbt(!container.shouldMatchNbt()), container::shouldMatchNbt);
			addChild(nbtButton);
		}

		width = Math.max(slotsPerRow * 18, getMaxButtonWidth());
		height = (fullSlotRows + (slotsInExtraRow > 0 ? 1 : 0)) * 18 + slotsTopYOffset;

		setDurabilityAndNbtButtonsVisibility();
	}

	private void setDurabilityAndNbtButtonsVisibility() {
		boolean visible = container.getPrimaryMatch() != PrimaryMatch.TAGS;
		if (nbtButton != null) {
			nbtButton.setVisible(visible);
		}
		if (durabilityButton != null) {
			durabilityButton.setVisible(visible);
		}
	}

	protected void onTagsMatchSelected() {
		//noop
	}

	private void addTagButtons() {
		tagButtonsYOffset = slotsTopYOffset + (getTagListHeight());
		addChild(new TagButton(new Position(x + 36, y + tagButtonsYOffset), ButtonDefinitions.REMOVE_TAG, button -> {
			container.removeSelectedTag();
			updateTagListAndRemoveTooltips();
			updateAddTooltip();
		}, delta -> {
			if (delta < 0) {
				container.selectNextTagToRemove();
				updateTagListAndRemoveTooltips();
			} else {
				container.selectPreviousTagToRemove();
				updateTagListAndRemoveTooltips();
			}
		}) {
			@Override
			protected List<ITextProperties> getTooltip() {
				return removeTagTooltip;
			}
		});
		updateTagListAndRemoveTooltips();

		addChild(new TagButton(new Position(x + 18, y + tagButtonsYOffset), ButtonDefinitions.ADD_TAG, button -> {
			container.addSelectedTag();
			updateAddTooltip();
			updateTagListAndRemoveTooltips();
		}, delta -> {
			if (delta < 0) {
				container.selectNextTagToAdd();
				updateAddTooltip();
			} else {
				container.selectPreviousTagToAdd();
				updateAddTooltip();
			}
		}) {
			@Override
			protected List<ITextProperties> getTooltip() {
				return addTagTooltip;
			}
		});
		updateAddTooltip();
		container.getTagSelectionSlot().setOnUpdate(this::updateAddTooltip);

		addChild(new ToggleButton<Boolean>(new Position(x + 54, y + tagButtonsYOffset), ButtonDefinitions.MATCH_ANY_TAG, button -> container.setMatchAnyTag(!container.shouldMatchAnyTag()), container::shouldMatchAnyTag) {
			@Override
			protected void renderBg(MatrixStack matrixStack, Minecraft minecraft, int mouseX, int mouseY) {
				if (container.getPrimaryMatch() == PrimaryMatch.TAGS) {
					super.renderBg(matrixStack, minecraft, mouseX, mouseY);
				}
			}

			@Override
			protected void renderWidget(MatrixStack matrixStack, int mouseX, int mouseY, float partialTicks) {
				if (container.getPrimaryMatch() == PrimaryMatch.TAGS) {
					super.renderWidget(matrixStack, mouseX, mouseY, partialTicks);
				}
			}

			@Override
			public boolean isMouseOver(double mouseX, double mouseY) {
				return container.getPrimaryMatch() == PrimaryMatch.TAGS && super.isMouseOver(mouseX, mouseY);
			}
		});
	}

	private void updateTagListAndRemoveTooltips() {
		updateTagListTooltip();
		updateRemoveTooltip();
	}

	private void updateTagListTooltip() {
		tagListTooltip.clear();
		tagListTooltip.add(new TranslationTextComponent(TranslationHelper.translUpgradeKey("tag_list.title")).withStyle());
		Set<ResourceLocation> tagNames = container.getTagNames();
		if (tagNames.isEmpty()) {
			tagListTooltip.add(new TranslationTextComponent(TranslationHelper.translUpgradeKey("tag_list.empty")).withStyle(TextFormatting.DARK_GRAY));
			return;
		}

		for (ResourceLocation tagName : tagNames) {
			tagListTooltip.add(new StringTextComponent("> " + tagName.toString()).withStyle(TextFormatting.GRAY));
		}
	}

	private void updateRemoveTooltip() {
		removeTagTooltip.clear();
		removeTagTooltip.add(new TranslationTextComponent(TranslationHelper.translUpgradeButton("remove_tag")));
		Set<ResourceLocation> tagNames = container.getTagNames();
		if (tagNames.isEmpty()) {
			removeTagTooltip.add(new TranslationTextComponent(TranslationHelper.translUpgradeButton("remove_tag.empty")).withStyle(TextFormatting.RED));
			return;
		}

		int curIndex = 0;
		for (ResourceLocation tagName : tagNames) {
			if (curIndex == container.getSelectedTagToRemove()) {
				removeTagTooltip.add(new StringTextComponent("-> " + tagName.toString()).withStyle(TextFormatting.RED));
			} else {
				removeTagTooltip.add(new StringTextComponent("> " + tagName.toString()).withStyle(TextFormatting.GRAY));
			}
			curIndex++;
		}
		removeTagTooltip.add(new TranslationTextComponent(TranslationHelper.translUpgradeButton("remove_tag.controls")).withStyle(TextFormatting.ITALIC, TextFormatting.DARK_GRAY));
	}

	private void updateAddTooltip() {
		addTagTooltip.clear();
		addTagTooltip.add(new TranslationTextComponent(TranslationHelper.translUpgradeButton("add_tag")));
		if (container.getTagSelectionSlot().getItem().isEmpty()) {
			addTagTooltip.add(new TranslationTextComponent(TranslationHelper.translUpgradeButton("add_tag.no_item")).withStyle(TextFormatting.ITALIC, TextFormatting.DARK_GRAY));
			return;
		}
		Set<ResourceLocation> tagsToAdd = container.getTagsToAdd();
		int curIndex = 0;
		for (ResourceLocation tagName : tagsToAdd) {
			if (curIndex == container.getSelectedTagToAdd()) {
				addTagTooltip.add(new StringTextComponent("-> " + tagName.toString()).withStyle(TextFormatting.GREEN));
			} else {
				addTagTooltip.add(new StringTextComponent("> " + tagName.toString()).withStyle(TextFormatting.GRAY));
			}
			curIndex++;
		}
		if (tagsToAdd.isEmpty()) {
			addTagTooltip.add(new TranslationTextComponent(TranslationHelper.translUpgradeButton("add_tag.no_additional_tags")).withStyle(TextFormatting.ITALIC, TextFormatting.YELLOW));
		} else {
			addTagTooltip.add(new TranslationTextComponent(TranslationHelper.translUpgradeButton("add_tag.controls")).withStyle(TextFormatting.ITALIC, TextFormatting.DARK_GRAY));
		}
	}

	protected int getMaxButtonWidth() {
		int maxWidth = 0;
		for (BackpackWidget w : children) {
			int buttonWidth = w.getX() + w.getWidth() - x;
			if (buttonWidth > maxWidth) {
				maxWidth = buttonWidth;
			}
		}
		return maxWidth;
	}

	protected boolean shouldShow(MatchButton matchButton) {
		for (MatchButton showMatchButton : showMatchButtons) {
			if (showMatchButton == matchButton) {
				return true;
			}
		}
		return false;
	}

	public void moveSlotsToView() {
		if (container.getPrimaryMatch() == PrimaryMatch.TAGS) {
			Slot slot = container.getTagSelectionSlot();
			slot.x = x - screen.getGuiLeft() + 1;
			slot.y = y - screen.getGuiTop() + tagButtonsYOffset + 1;
			container.getFilterSlots().forEach(s -> s.x = BackpackScreen.DISABLED_SLOT_X_POS);
		} else {
			int upgradeSlotNumber = 0;
			for (S slot : container.getFilterSlots()) {
				slot.x = x - screen.getGuiLeft() + 1 + (upgradeSlotNumber % slotsPerRow) * 18;
				slot.y = y - screen.getGuiTop() + slotsTopYOffset + 1 + (upgradeSlotNumber / slotsPerRow) * 18;
				upgradeSlotNumber++;
			}
			container.getTagSelectionSlot().x = BackpackScreen.DISABLED_SLOT_X_POS;
		}
	}

	@Override
	protected void renderWidget(MatrixStack matrixStack, int mouseX, int mouseY, float partialTicks) {
		super.renderWidget(matrixStack, mouseX, mouseY, partialTicks);
		if (container.getPrimaryMatch() == PrimaryMatch.TAGS) {
			renderTagNames(matrixStack, mouseX, mouseY);
		}
	}

	private void renderTagNames(MatrixStack matrixStack, int mouseX, int mouseY) {
		int count = 0;
		int prefixWidth = font.width("...");
		Set<ResourceLocation> tagNames = container.getTagNames();
		int maxTagNameLines = getTagListHeight() / 10;
		for (ResourceLocation tagName : tagNames) {
			if (tagNames.size() > maxTagNameLines && count == maxTagNameLines - 1) {
				font.draw(matrixStack, new TranslationTextComponent(TranslationHelper.translUpgradeKey("tag_list.tag_overflow"), String.valueOf(tagNames.size() - (maxTagNameLines - 1))), (float) x + 2, (float) y + 23 + count * 10, MORE_TAGS_FONT_COLOR);
				break;
			}
			String name = tagName.toString();
			String shortened = name;
			if (font.width(name) > MAX_TAG_NAME_WIDTH) {
				shortened = font.plainSubstrByWidth(name, MAX_TAG_NAME_WIDTH - prefixWidth, true);
				if (!shortened.equals(name)) {
					shortened = "..." + shortened;
				}
			}
			font.draw(matrixStack, shortened, (float) x + 2, (float) y + 23 + count * 10, TAG_FONT_COLOR);
			count++;
		}
		if (isMouseOverTagList(mouseX, mouseY)) {
			GuiHelper.setTooltipToRender(tagListTooltip);
		}
	}

	private int getTagListHeight() {
		return (totalSlotRows - 1) * 18;
	}

	private boolean isMouseOverTagList(double mouseX, double mouseY) {
		return mouseX >= x && mouseX < x + getTagListWidth() && mouseY >= y + slotsTopYOffset && mouseY < y + slotsTopYOffset + getTagListHeight();
	}

	@Override
	public int getWidth() {
		return width;
	}

	@Override
	public int getHeight() {
		return height;
	}

	@Override
	protected void renderBg(MatrixStack matrixStack, Minecraft minecraft, int mouseX, int mouseY) {
		if (container.getPrimaryMatch() != PrimaryMatch.TAGS) {
			GuiHelper.renderSlotsBackground(minecraft, matrixStack, x, y + slotsTopYOffset, slotsPerRow, fullSlotRows, slotsInExtraRow);
		} else {
			GuiHelper.renderSlotsBackground(minecraft, matrixStack, x, y + tagButtonsYOffset, 1, 1, 0);
			renderTagListBackground(matrixStack, minecraft);
		}
	}

	private void renderTagListBackground(MatrixStack matrixStack, Minecraft minecraft) {
		minecraft.getTextureManager().bind(GuiHelper.GUI_CONTROLS);

		int u = 29;
		int v = 146;
		int renderWidth = getTagListWidth();
		int renderHeight = getTagListHeight();
		int textureBgWidth = 66;
		int textureBgHeight = 56;
		int halfWidth = renderWidth / 2;
		int halfHeight = renderHeight / 2;
		blit(matrixStack, x, y + slotsTopYOffset, u, v, halfWidth, halfHeight, TEXTURE_WIDTH, TEXTURE_HEIGHT);
		blit(matrixStack, x, y + slotsTopYOffset + halfHeight, u, (float) v + textureBgHeight - halfHeight, halfWidth, halfHeight, TEXTURE_WIDTH, TEXTURE_HEIGHT);
		blit(matrixStack, x + halfWidth, y + slotsTopYOffset, (float) u + textureBgWidth - halfWidth, v, halfWidth, halfHeight, TEXTURE_WIDTH, TEXTURE_HEIGHT);
		blit(matrixStack, x + halfWidth, y + slotsTopYOffset + halfHeight, (float) u + textureBgWidth - halfWidth, (float) v + textureBgHeight - halfHeight, halfWidth, halfHeight, TEXTURE_WIDTH, TEXTURE_HEIGHT);
	}

	private int getTagListWidth() {
		return slotsPerRow * 18;
	}

	private class TagButton extends Button {
		private final DoubleConsumer onScroll;

		public TagButton(Position position, ButtonDefinition buttonDefinition, IntConsumer onClick, DoubleConsumer onScroll) {
			super(position, buttonDefinition, onClick);
			this.onScroll = onScroll;
		}

		@Override
		protected void renderBg(MatrixStack matrixStack, Minecraft minecraft, int mouseX, int mouseY) {
			if (container.getPrimaryMatch() == PrimaryMatch.TAGS) {
				super.renderBg(matrixStack, minecraft, mouseX, mouseY);
			}
		}

		@Override
		protected void renderWidget(MatrixStack matrixStack, int mouseX, int mouseY, float partialTicks) {
			if (container.getPrimaryMatch() == PrimaryMatch.TAGS) {
				super.renderWidget(matrixStack, mouseX, mouseY, partialTicks);
			}
		}

		@Override
		public boolean isMouseOver(double mouseX, double mouseY) {
			return container.getPrimaryMatch() == PrimaryMatch.TAGS && super.isMouseOver(mouseX, mouseY);
		}

		@Override
		public boolean mouseScrolled(double pMouseX, double pMouseY, double pDelta) {
			if (container.getPrimaryMatch() != PrimaryMatch.TAGS) {
				return false;
			}
			onScroll.accept(pDelta);
			return true;
		}
	}

	public enum MatchButton {
		ALLOW_LIST,
		PRIMARY_MATCH,
		DURABILITY,
		NBT
	}
}
