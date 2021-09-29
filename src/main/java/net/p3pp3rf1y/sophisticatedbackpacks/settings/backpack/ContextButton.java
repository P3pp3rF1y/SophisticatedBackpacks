package net.p3pp3rf1y.sophisticatedbackpacks.settings.backpack;

import com.mojang.blaze3d.vertex.PoseStack;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.narration.NarratedElementType;
import net.minecraft.client.gui.narration.NarrationElementOutput;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TranslatableComponent;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ButtonBase;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Dimension;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.GuiHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Position;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TextureBlitData;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.UV;

import java.util.List;
import java.util.function.IntConsumer;
import java.util.function.Supplier;

import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.GuiHelper.GUI_CONTROLS;

public class ContextButton extends ButtonBase {
	public static final TextureBlitData LEFT_BUTTON_HOVERED_BACKGROUND = new TextureBlitData(GUI_CONTROLS, new UV(47, 0), new Dimension(16, 18));
	public static final TextureBlitData LEFT_BUTTON_BACKGROUND = new TextureBlitData(GUI_CONTROLS, new UV(29, 0), new Dimension(16, 18));
	public static final TextureBlitData MIDDLE_BUTTON_HOVERED_BACKGROUND = new TextureBlitData(GUI_CONTROLS, new UV(49, 0), new Dimension(14, 18));
	public static final TextureBlitData MIDDLE_BUTTON_BACKGROUND = new TextureBlitData(GUI_CONTROLS, new UV(31, 0), new Dimension(14, 18));
	public static final TextureBlitData RIGHT_BUTTON_HOVERED_BACKGROUND = new TextureBlitData(GUI_CONTROLS, new UV(49, 0), new Dimension(16, 18));
	public static final TextureBlitData RIGHT_BUTTON_BACKGROUND = new TextureBlitData(GUI_CONTROLS, new UV(31, 0), new Dimension(16, 18));
	private final Supplier<Component> getTitle;
	private final Supplier<List<Component>> getTooltipKey;

	protected ContextButton(Position position, IntConsumer onClick, Supplier<Component> getTitle, Supplier<List<Component>> getTooltipKey) {
		super(position, new Dimension(62, 18), onClick);
		this.getTitle = getTitle;
		this.getTooltipKey = getTooltipKey;
	}

	@Override
	protected void renderBg(PoseStack matrixStack, Minecraft minecraft, int mouseX, int mouseY) {
		if (isMouseOver(mouseX, mouseY)) {
			renderBackground(matrixStack, minecraft, LEFT_BUTTON_HOVERED_BACKGROUND, MIDDLE_BUTTON_HOVERED_BACKGROUND, RIGHT_BUTTON_HOVERED_BACKGROUND);
			GuiHelper.setTooltipToRender(getTooltipKey.get());
		} else {
			renderBackground(matrixStack, minecraft, LEFT_BUTTON_BACKGROUND, MIDDLE_BUTTON_BACKGROUND, RIGHT_BUTTON_BACKGROUND);
		}
	}

	private void renderBackground(PoseStack matrixStack, Minecraft minecraft, TextureBlitData leftButtonHoveredBackground, TextureBlitData middleButtonHoveredBackground, TextureBlitData rightButtonHoveredBackground) {
		int left = x;
		GuiHelper.blit(minecraft, matrixStack, left, y, leftButtonHoveredBackground);
		left += leftButtonHoveredBackground.getWidth();
		GuiHelper.blit(minecraft, matrixStack, left, y, middleButtonHoveredBackground);
		left += middleButtonHoveredBackground.getWidth();
		GuiHelper.blit(minecraft, matrixStack, left, y, middleButtonHoveredBackground);
		left += middleButtonHoveredBackground.getWidth();
		GuiHelper.blit(minecraft, matrixStack, left, y, rightButtonHoveredBackground);
	}

	@Override
	protected void renderWidget(PoseStack matrixStack, int mouseX, int mouseY, float partialTicks) {
		drawCenteredString(matrixStack, Minecraft.getInstance().font, getTitle.get(), x + getWidth() / 2, y - 4 + getHeight() / 2, 16777215 | (255 << 24));
	}

	@Override
	public void updateNarration(NarrationElementOutput pNarrationElementOutput) {
		pNarrationElementOutput.add(NarratedElementType.TITLE, new TranslatableComponent("gui.sophisticatedbackpacks.narrate.context_button", getTitle.get()));
		pNarrationElementOutput.add(NarratedElementType.USAGE, new TranslatableComponent("gui.sophisticatedbackpacks.narrate.context_button.usage"));
	}
}
