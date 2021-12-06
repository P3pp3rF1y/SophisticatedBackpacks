package net.p3pp3rf1y.sophisticatedbackpacks.settings;

import com.google.common.collect.ImmutableList;
import com.mojang.blaze3d.systems.RenderSystem;
import com.mojang.blaze3d.vertex.PoseStack;
import net.minecraft.ChatFormatting;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.narration.NarrationElementOutput;
import net.minecraft.client.gui.screens.Screen;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TranslatableComponent;
import net.minecraft.world.item.DyeColor;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ButtonBase;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Dimension;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.GuiHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Position;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TranslationHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.ColorHelper;

import java.util.List;
import java.util.Optional;
import java.util.function.Consumer;
import java.util.function.Supplier;

import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.GuiHelper.DEFAULT_BUTTON_BACKGROUND;
import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.GuiHelper.DEFAULT_BUTTON_HOVERED_BACKGROUND;

public class ColorToggleButton extends ButtonBase {
	private static final DyeColor[] DYE_VALUES = DyeColor.values();
	private static final List<Component> TOOLTIP = new ImmutableList.Builder<Component>()
			.add(new TranslatableComponent(TranslationHelper.translSettingsButton("toggle_color")))
			.addAll(TranslationHelper.getTranslatedLines(TranslationHelper.translSettingsButton("toggle_color_detail"), null, ChatFormatting.GRAY))
			.build();

	private final Supplier<DyeColor> getColor;
	private final Consumer<DyeColor> setColor;

	public ColorToggleButton(Position position, Supplier<DyeColor> getColor, Consumer<DyeColor> setColor) {
		super(position, Dimension.SQUARE_18, b -> {});
		this.getColor = getColor;
		this.setColor = setColor;
		setOnClick(this::onClick);
	}

	private void onClick(int button) {
		toggleColor(button);
	}

	private void toggleColor(int button) {
		if (button == 0) {
			setColor.accept(nextColor(getColor.get()));
		} else if (button == 1) {
			setColor.accept(previousColor(getColor.get()));
		}
	}

	private DyeColor nextColor(DyeColor color) {
		return DYE_VALUES[(color.ordinal() + 1) % DYE_VALUES.length];
	}

	private DyeColor previousColor(DyeColor color) {
		return DYE_VALUES[(color.ordinal() - 1 + DYE_VALUES.length) % DYE_VALUES.length];
	}

	@Override
	protected void renderBg(PoseStack matrixStack, Minecraft minecraft, int mouseX, int mouseY) {
		if (isMouseOver(mouseX, mouseY)) {
			GuiHelper.blit(matrixStack, x, y, DEFAULT_BUTTON_HOVERED_BACKGROUND);
		} else {
			GuiHelper.blit(matrixStack, x, y, DEFAULT_BUTTON_BACKGROUND);
		}
	}

	@Override
	protected void renderWidget(PoseStack matrixStack, int mouseX, int mouseY, float partialTicks) {
		RenderSystem.disableDepthTest();
		RenderSystem.colorMask(true, true, true, false);
		int color = ColorHelper.getColor(getColor.get().getTextureDiffuseColors()) | (200 << 24);
		fillGradient(matrixStack, x + 3, y + 3, x + 15, y + 15, color, color);
		RenderSystem.colorMask(true, true, true, true);
		RenderSystem.enableDepthTest();
	}

	@Override
	public void renderTooltip(Screen screen, PoseStack poseStack, int mouseX, int mouseY) {
		super.renderTooltip(screen, poseStack, mouseX, mouseY);
		if (isMouseOver(mouseX, mouseY)) {
			screen.renderTooltip(poseStack, TOOLTIP, Optional.empty(), mouseX, mouseY);
		}
	}

	@Override
	public void updateNarration(NarrationElementOutput pNarrationElementOutput) {
		//TODO add narration
	}
}
