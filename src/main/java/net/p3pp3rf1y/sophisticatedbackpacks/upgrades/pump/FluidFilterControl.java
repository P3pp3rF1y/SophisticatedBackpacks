package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.pump;

import com.mojang.blaze3d.matrix.MatrixStack;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.texture.TextureAtlasSprite;
import net.minecraft.fluid.Fluid;
import net.minecraft.fluid.Fluids;
import net.minecraft.inventory.container.PlayerContainer;
import net.minecraft.util.ResourceLocation;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.BackpackWidget;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.GuiHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Position;

import java.util.Optional;

public class FluidFilterControl extends BackpackWidget {
	private final FluidFilterContainer container;

	protected FluidFilterControl(Position position, FluidFilterContainer container) {
		super(position);
		this.container = container;
	}

	@Override
	protected void renderBg(MatrixStack matrixStack, Minecraft minecraft, int mouseX, int mouseY) {
		GuiHelper.renderSlotsBackground(minecraft, matrixStack, x, y, container.getNumberOfFluidFilters(), 1);
	}

	@Override
	protected void renderWidget(MatrixStack matrixStack, int mouseX, int mouseY, float partialTicks) {
		for (int i = 0; i < container.getNumberOfFluidFilters(); i++) {
			Fluid fluid = container.getFluid(i);
			if (fluid != Fluids.EMPTY) {
				ResourceLocation texture = fluid.getAttributes().getStillTexture();
				TextureAtlasSprite still = minecraft.getTextureAtlas(PlayerContainer.BLOCK_ATLAS).apply(texture);
				GuiHelper.renderTiledFluidTextureAtlas(matrixStack, still, fluid.getAttributes().getColor(), x + i * 18 + 1, y + 1, 16, minecraft);
			}
		}
	}

	@Override
	public boolean mouseClicked(double mouseX, double mouseY, int pButton) {
		if (!isMouseOver(mouseX, mouseY)) {
			return false;
		}

		getSlotClicked(mouseX, mouseY).ifPresent(container::slotClick);

		return true;
	}

	private Optional<Integer> getSlotClicked(double mouseX, double mouseY) {
		if (mouseY < y + 1 || mouseY >= y + 17) {
			return Optional.empty();
		}
		int index = (int) ((mouseX - x) / 18);
		return Optional.of(index);
	}

	@Override
	public int getWidth() {
		return container.getNumberOfFluidFilters() * 18;
	}

	@Override
	public int getHeight() {
		return 18;
	}
}
