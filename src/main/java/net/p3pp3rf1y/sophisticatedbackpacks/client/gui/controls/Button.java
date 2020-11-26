package net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls;

import com.mojang.blaze3d.matrix.MatrixStack;
import net.minecraft.client.Minecraft;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.GuiHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.Position;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.TextureBlitData;

import javax.annotation.Nullable;
import java.util.function.Consumer;

@OnlyIn(Dist.CLIENT)
public class Button extends ButtonBase {
	private final TextureBlitData backgroundTexture;
	@Nullable
	private final TextureBlitData hoveredBackgroundTexture;
	@Nullable
	private TextureBlitData foregroundTexture;

	public Button(Position position, ButtonDefinition buttonDefinition, Consumer<Integer> onClick) {
		this(position, buttonDefinition, onClick, null);
	}

	public void setForegroundTexture(TextureBlitData foregroundTexture) {
		this.foregroundTexture = foregroundTexture;
	}

	public Button(Position position, ButtonDefinition buttonDefinition, Consumer<Integer> onClick, @Nullable TextureBlitData foregroundTexture) {
		super(position, buttonDefinition.getDimension(), onClick);
		backgroundTexture = buttonDefinition.getBackgroundTexture();
		this.foregroundTexture = foregroundTexture;
		hoveredBackgroundTexture = buttonDefinition.getHoveredBackgroundTexture();
	}

	@Override
	protected void renderBg(MatrixStack matrixStack, Minecraft minecraft, int mouseX, int mouseY) {
		if (isMouseOver(mouseX, mouseY)) {
			if (hoveredBackgroundTexture != null) {
				GuiHelper.blit(minecraft, matrixStack, x, y, hoveredBackgroundTexture);
			}
		} else {
			GuiHelper.blit(minecraft, matrixStack, x, y, backgroundTexture);
		}
	}

	@Override
	protected void renderWidget(MatrixStack matrixStack, int mouseX, int mouseY, float partialTicks) {
		if (foregroundTexture != null) {
			GuiHelper.blit(minecraft, matrixStack, x, y, foregroundTexture);
		}
	}
}
