package net.p3pp3rf1y.sophisticatedbackpacks.client.gui;

import com.mojang.blaze3d.matrix.MatrixStack;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.DialogTexts;
import net.minecraft.client.gui.FontRenderer;
import net.minecraft.client.gui.screen.Screen;
import net.minecraft.client.gui.screen.inventory.ContainerScreen;
import net.minecraft.client.gui.widget.button.Button;
import net.minecraft.entity.player.PlayerInventory;
import net.minecraft.util.text.ITextComponent;
import net.minecraft.util.text.StringTextComponent;
import net.minecraft.util.text.TranslationTextComponent;
import net.minecraftforge.fml.client.gui.GuiUtils;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackSettingsManager;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.SettingsContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.network.BackpackOpenMessage;
import net.p3pp3rf1y.sophisticatedbackpacks.network.PacketHandler;

import javax.annotation.Nullable;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.function.BooleanSupplier;

import static net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackSettingsManager.KEEP_TAB_OPEN;
import static net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackSettingsManager.SHIFT_CLICK_INTO_OPEN_TAB_FIRST;

public class SettingsScreen extends ContainerScreen<SettingsContainer> {
	private boolean showPlayerSettings = true;

	public SettingsScreen(SettingsContainer screenContainer, PlayerInventory inv, ITextComponent titleIn) {
		super(screenContainer, inv, titleIn);
	}

	private final Set<Runnable> onLevelChangeListeners = new HashSet<>();

	@Override
	protected void init() {
		super.init();
		addButton(new BooleanButton(this, width / 2 - 75, 30,
				new ButtonTranslation("settings.player", "settings.backpack", "settings.player.tooltip", "settings.backpack.tooltip"),
				() -> showPlayerSettings, button -> {
			showPlayerSettings = !showPlayerSettings;
			onLevelChangeListeners.forEach(Runnable::run);
		}));
		BooleanButton shiftClickButton = new BooleanButton(this, width / 2 - 155, 60, new BooleanSettingButtonTranslation("shift_click_open_tab", true),
				() -> getBackpackSetting(SHIFT_CLICK_INTO_OPEN_TAB_FIRST), button -> setBackpackSetting(SHIFT_CLICK_INTO_OPEN_TAB_FIRST, !getBackpackSetting(SHIFT_CLICK_INTO_OPEN_TAB_FIRST)));
		addButton(shiftClickButton);
		onLevelChangeListeners.add(shiftClickButton::updateMessage);
		BooleanButton keepTabOpenButton = new BooleanButton(this, width / 2 + 5, 60, new BooleanSettingButtonTranslation("keep_tab_open", true),
				() -> getBackpackSetting(KEEP_TAB_OPEN), button -> setBackpackSetting(KEEP_TAB_OPEN, !getBackpackSetting(KEEP_TAB_OPEN)));
		addButton(keepTabOpenButton);
		onLevelChangeListeners.add(keepTabOpenButton::updateMessage);
		addButton(new Button(width / 2 - 100, 100, 200, 20, DialogTexts.GUI_DONE, button -> PacketHandler.sendToServer(new BackpackOpenMessage())));
	}

	private <T> T getBackpackSetting(BackpackSettingsManager.BackpackSetting<T> setting) {
		return getContainer().getBackpackSetting(showPlayerSettings, setting);
	}

	private <T> void setBackpackSetting(BackpackSettingsManager.BackpackSetting<T> setting, T value) {
		getContainer().setBackpackSetting(showPlayerSettings, setting, value);
	}

	@Override
	protected void drawGuiContainerBackgroundLayer(MatrixStack matrixStack, float partialTicks, int x, int y) {
		//noop
	}

	@Override
	public boolean keyPressed(int keyCode, int scanCode, int modifiers) {
		if (keyCode == 256) {
			PacketHandler.sendToServer(new BackpackOpenMessage());
			return true;
		}
		return super.keyPressed(keyCode, scanCode, modifiers);
	}

	public static SettingsScreen constructScreen(SettingsContainer screenContainer, PlayerInventory playerInventory, ITextComponent title) {
		return new SettingsScreen(screenContainer, playerInventory, title);
	}

	@Override
	public void render(MatrixStack matrixStack, int mouseX, int mouseY, float partialTicks) {
		renderBackground(matrixStack);
		drawCenteredString(matrixStack, font, title, width / 2, 15, 16777215);
		super.render(matrixStack, mouseX, mouseY, partialTicks);
	}

	@Override
	protected void drawGuiContainerForegroundLayer(MatrixStack matrixStack, int x, int y) {
		//noop
	}

	private static class BooleanButton extends Button {
		private final ITextComponent offTitle;
		private final ITextComponent onTitle;
		@Nullable
		private final ITextComponent offValueTooltip;
		@Nullable
		private final ITextComponent onValueTooltip;
		private final IPressable pressAction;
		private final Screen screen;
		private final BooleanSupplier isOn;
		private List<ITextComponent> tooltip;
		private final FontRenderer font = Minecraft.getInstance().fontRenderer;

		public BooleanButton(Screen screen, int x, int y,
				ButtonTranslation buttonTranslation, BooleanSupplier isOn, IPressable pressAction) {
			super(x, y, 150, 20, StringTextComponent.EMPTY, button -> {});
			this.screen = screen;
			this.isOn = isOn;

			offTitle = new TranslationTextComponent(TranslationHelper.translGui(buttonTranslation.getOffTitle()));
			onTitle = new TranslationTextComponent(TranslationHelper.translGui(buttonTranslation.getOnTitle()));
			offValueTooltip = buttonTranslation.getOffValueTooltip() == null ? null : new TranslationTextComponent(TranslationHelper.translGui(buttonTranslation.getOffValueTooltip()));
			onValueTooltip = buttonTranslation.getOnValueTooltip() == null ? null : new TranslationTextComponent(TranslationHelper.translGui(buttonTranslation.getOnValueTooltip()));
			this.pressAction = pressAction;
			updateMessage();
		}

		private void updateMessage() {
			setMessage(isOn.getAsBoolean() ? onTitle : offTitle);
			ITextComponent t = isOn.getAsBoolean() ? onValueTooltip : offValueTooltip;
			tooltip = t == null ? Collections.emptyList() : Collections.singletonList(t);
		}

		@Override
		public void onPress() {
			pressAction.onPress(this);
			updateMessage();
		}

		@Override
		public void renderToolTip(MatrixStack matrixStack, int mouseX, int mouseY) {
			if (!tooltip.isEmpty()) {
				GuiUtils.drawHoveringText(matrixStack, tooltip, mouseX, mouseY, screen.width, screen.height, -1, font);
			}
		}
	}

	public static class BooleanSettingButtonTranslation extends ButtonTranslation {
		private static final String SETTINGS_PREFIX = "settings.";

		public BooleanSettingButtonTranslation(String settingName, boolean hasTooltip) {
			super(SETTINGS_PREFIX + settingName + ".on", SETTINGS_PREFIX + settingName + ".off",
					hasTooltip ? SETTINGS_PREFIX + settingName + ".on.tooltip" : null, hasTooltip ? SETTINGS_PREFIX + settingName + ".off.tooltip" : null);
		}
	}

	public static class ButtonTranslation {
		private final String onTitle;
		private final String offTitle;
		@Nullable
		private final String onValueTooltip;
		@Nullable
		private final String offValueTooltip;

		public ButtonTranslation(String onTitle, String offTitle, @Nullable String onValueTooltip, @Nullable String offValueTooltip) {
			this.onTitle = onTitle;
			this.offTitle = offTitle;
			this.onValueTooltip = onValueTooltip;
			this.offValueTooltip = offValueTooltip;
		}

		public String getOnTitle() {
			return onTitle;
		}

		public String getOffTitle() {
			return offTitle;
		}

		@Nullable
		public String getOnValueTooltip() {
			return onValueTooltip;
		}

		@Nullable
		public String getOffValueTooltip() {
			return offValueTooltip;
		}
	}
}
