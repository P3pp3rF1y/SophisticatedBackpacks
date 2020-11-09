package net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls;

import com.mojang.blaze3d.matrix.MatrixStack;
import net.minecraft.client.Minecraft;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.GuiHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.TextureBlitData;

import javax.annotation.Nullable;
import java.util.function.Predicate;

public class Button extends ButtonBase {
	private final TextureBlitData backgroundTexture;
	private final TextureBlitData foregroundTexture;

	public Button(int x, int y, int width, int height, Predicate<Integer> onClick, TextureBlitData backgroundTexture) {
		this(x, y, width, height, onClick, backgroundTexture, null);
	}

	public Button(int x, int y, int width, int height, Predicate<Integer> onClick, TextureBlitData backgroundTexture,
			@Nullable TextureBlitData foregroundTexture) {
		super(x, y, width, height, onClick);
		this.backgroundTexture = backgroundTexture;
		this.foregroundTexture = foregroundTexture;
	}

	@Override
	protected void renderBg(MatrixStack matrixStack, Minecraft minecraft, int mouseX, int mouseY) {
		GuiHelper.blit(minecraft, matrixStack, x, y, backgroundTexture);
	}

	@Override
	protected void renderWidget(MatrixStack matrixStack, int mouseX, int mouseY, float partialTicks) {
		if (foregroundTexture != null) {
			GuiHelper.blit(minecraft, matrixStack, x, y, foregroundTexture);
		}
	}

}
