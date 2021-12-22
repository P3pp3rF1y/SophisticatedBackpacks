package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.xppump;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.mojang.blaze3d.matrix.MatrixStack;
import net.minecraft.client.Minecraft;
import net.minecraft.item.DyeColor;
import net.minecraft.util.text.ITextComponent;
import net.minecraft.util.text.StringTextComponent;
import net.minecraft.util.text.TextFormatting;
import net.minecraft.util.text.TranslationTextComponent;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.UpgradeSettingsTab;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.BackpackWidget;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.Button;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ButtonDefinition;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ToggleButton;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Dimension;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.GuiHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Position;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TextureBlitData;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TranslationHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.UV;

import java.util.List;
import java.util.function.DoubleConsumer;
import java.util.function.Supplier;

import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ButtonDefinitions.createToggleButtonDefinition;
import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.GuiHelper.DEFAULT_BUTTON_BACKGROUND;
import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.GuiHelper.DEFAULT_BUTTON_HOVERED_BACKGROUND;
import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TranslationHelper.*;

public class XpPumpUpgradeTab extends UpgradeSettingsTab<XpPumpUpgradeContainer> {
	private static final ButtonDefinition.Toggle<AutomationDirection> DIRECTION = createToggleButtonDefinition(
			ImmutableMap.of(
					AutomationDirection.INPUT, GuiHelper.getButtonStateData(new UV(144, 0), translUpgradeButton("xp_pump_input"), Dimension.SQUARE_16, new Position(1, 1)),
					AutomationDirection.OUTPUT, GuiHelper.getButtonStateData(new UV(128, 16), translUpgradeButton("xp_pump_output"), Dimension.SQUARE_16, new Position(1, 1)),
					AutomationDirection.OFF, GuiHelper.getButtonStateData(new UV(240, 0), translUpgradeButton("xp_pump_off"), Dimension.SQUARE_16, new Position(1, 1))
			));
	private static final ButtonDefinition.Toggle<Boolean> MEND_ITEMS = createToggleButtonDefinition(
			ImmutableMap.of(
					true, GuiHelper.getButtonStateData(new UV(144, 32), translUpgradeButton("mend_items"), Dimension.SQUARE_16, new Position(1, 1)),
					false, GuiHelper.getButtonStateData(new UV(160, 32), translUpgradeButton("do_not_mend_items"), Dimension.SQUARE_16, new Position(1, 1))
			));
	private static final TextureBlitData STORE_ALL_FOREGROUND = new TextureBlitData(GuiHelper.ICONS, new Position(1, 1), Dimension.SQUARE_256, new UV(192, 16), Dimension.SQUARE_16);
	public static final ButtonDefinition STORE_ALL = new ButtonDefinition(Dimension.SQUARE_18, DEFAULT_BUTTON_BACKGROUND, DEFAULT_BUTTON_HOVERED_BACKGROUND, STORE_ALL_FOREGROUND,
			new TranslationTextComponent(TranslationHelper.translUpgradeButton("store_all_experience")));
	private static final TextureBlitData TAKE_ALL_FOREGROUND = new TextureBlitData(GuiHelper.ICONS, new Position(1, 1), Dimension.SQUARE_256, new UV(144, 16), Dimension.SQUARE_16);
	public static final ButtonDefinition TAKE_ALL = new ButtonDefinition(Dimension.SQUARE_18, DEFAULT_BUTTON_BACKGROUND, DEFAULT_BUTTON_HOVERED_BACKGROUND, TAKE_ALL_FOREGROUND,
			new TranslationTextComponent(TranslationHelper.translUpgradeButton("take_all_experience")));
	private static final TextureBlitData TAKE_FOREGROUND = new TextureBlitData(GuiHelper.ICONS, new Position(1, 1), Dimension.SQUARE_256, new UV(160, 16), Dimension.SQUARE_16);
	public static final ButtonDefinition TAKE = new ButtonDefinition(Dimension.SQUARE_18, DEFAULT_BUTTON_BACKGROUND, DEFAULT_BUTTON_HOVERED_BACKGROUND, TAKE_FOREGROUND);
	private static final TextureBlitData STORE_FOREGROUND = new TextureBlitData(GuiHelper.ICONS, new Position(1, 1), Dimension.SQUARE_256, new UV(176, 16), Dimension.SQUARE_16);
	public static final ButtonDefinition STORE = new ButtonDefinition(Dimension.SQUARE_18, DEFAULT_BUTTON_BACKGROUND, DEFAULT_BUTTON_HOVERED_BACKGROUND, STORE_FOREGROUND);
	private final Button takeButton;
	private final Button storeButton;

	public XpPumpUpgradeTab(XpPumpUpgradeContainer upgradeContainer, Position position, BackpackScreen screen) {
		super(upgradeContainer, position, screen, translUpgrade("xp_pump"), translUpgradeTooltip("xp_pump"));
		int currentYOffset = 24;
		addHideableChild(new ToggleButton<>(new Position(x + 3, y + currentYOffset), DIRECTION,
				button -> getContainer().setDirection(getContainer().getDirection().next()),
				() -> getContainer().getDirection()));
		addHideableChild(new LevelSelector(new Position(x + 21, y + currentYOffset), () -> String.valueOf(upgradeContainer.getLevel()), delta ->
				upgradeContainer.setLevel(upgradeContainer.getLevel() + (delta > 0 ? 1 : -1))));
		currentYOffset += 20;

		if (Config.COMMON.xpPumpUpgrade.mendingOn.get()) {
			addHideableChild(new ToggleButton<>(new Position(x + 3, y + currentYOffset), MEND_ITEMS, button -> upgradeContainer.setMendItems(!upgradeContainer.shouldMendItems()), upgradeContainer::shouldMendItems));
			currentYOffset += 20;
		}

		addHideableChild(new Button(new Position(x + 3, y + currentYOffset), TAKE_ALL, button -> upgradeContainer.takeAllExperience()));
		takeButton = new Button(new Position(x + 21, y + currentYOffset), TAKE, button -> upgradeContainer.takeLevels()) {
			@Override
			public boolean mouseScrolled(double mouseX, double mouseY, double delta) {
				upgradeContainer.setLevelsToTake(upgradeContainer.getLevelsToTake() + (delta > 0 ? 1 : -1));
				setTakeTooltip();
				return true;
			}
		};
		setTakeTooltip();
		addHideableChild(takeButton);
		storeButton = new Button(new Position(x + 39, y + currentYOffset), STORE, button -> upgradeContainer.storeLevels()) {
			@Override
			public boolean mouseScrolled(double mouseX, double mouseY, double delta) {
				upgradeContainer.setLevelsToStore(upgradeContainer.getLevelsToStore() + (delta > 0 ? 1 : -1));
				setStoreTooltip();
				return true;
			}
		};
		setStoreTooltip();
		addHideableChild(storeButton);
		addHideableChild(new Button(new Position(x + 57, y + currentYOffset), STORE_ALL, button -> upgradeContainer.storeAllExperience()));

	}

	private void setStoreTooltip() {
		storeButton.setTooltip(ImmutableList.of(
				new TranslationTextComponent(TranslationHelper.translUpgradeButton("store_levels"), new StringTextComponent(String.valueOf(getContainer().getLevelsToStore())).withStyle(TextFormatting.RED)),
				new TranslationTextComponent(TranslationHelper.translUpgradeButton("store_levels.controls")).withStyle(TextFormatting.ITALIC, TextFormatting.DARK_GRAY))
		);
	}

	private void setTakeTooltip() {
		takeButton.setTooltip(ImmutableList.of(
				new TranslationTextComponent(TranslationHelper.translUpgradeButton("take_levels"), new StringTextComponent(String.valueOf(getContainer().getLevelsToTake())).withStyle(TextFormatting.GREEN)),
				new TranslationTextComponent(TranslationHelper.translUpgradeButton("take_levels.controls")).withStyle(TextFormatting.ITALIC, TextFormatting.DARK_GRAY))
		);
	}

	@Override
	protected void moveSlotsToTab() {
		//noop
	}

	private static class LevelSelector extends BackpackWidget {
		private final Supplier<String> getText;
		private final DoubleConsumer onScroll;

		private static final List<ITextComponent> TOOLTIP = ImmutableList.of(
				new TranslationTextComponent(translUpgradeControl("xp_level_select.tooltip")),
				new TranslationTextComponent(translUpgradeControl("xp_level_select.tooltip.controls")).withStyle(TextFormatting.ITALIC, TextFormatting.DARK_GRAY));

		protected LevelSelector(Position position, Supplier<String> getText, DoubleConsumer onScroll) {
			super(position);
			this.getText = getText;
			this.onScroll = onScroll;
		}

		@Override
		protected void renderBg(MatrixStack matrixStack, Minecraft minecraft, int mouseX, int mouseY) {
			GuiHelper.renderControlBackground(matrixStack, minecraft, x, y, 54, 18);
		}

		@Override
		protected void renderWidget(MatrixStack matrixStack, int mouseX, int mouseY, float partialTicks) {
			String text = getText.get();
			ITextComponent fullText = new TranslationTextComponent(translUpgradeControl("xp_level_select"), new StringTextComponent(text).withStyle(TextFormatting.WHITE)).withStyle(TextFormatting.GRAY);
			int xOffset = (getWidth() - minecraft.font.width(fullText)) / 2;
			int yOffset = (int) Math.ceil((getHeight() - minecraft.font.lineHeight) / 2d);
			minecraft.font.draw(matrixStack, fullText, (float) x + xOffset, (float) y + yOffset, DyeColor.BLACK.getTextColor());

			if (isMouseOver(mouseX, mouseY)) {
				GuiHelper.setTooltipToRender(TOOLTIP);
			}
		}

		@Override
		public boolean mouseScrolled(double pMouseX, double pMouseY, double pDelta) {
			onScroll.accept(pDelta);
			return true;
		}

		@Override
		public int getWidth() {
			return 54;
		}

		@Override
		public int getHeight() {
			return 18;
		}
	}
}
