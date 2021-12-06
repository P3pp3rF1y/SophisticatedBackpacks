package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.pump;

import com.mojang.blaze3d.vertex.PoseStack;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.narration.NarrationElementOutput;
import net.minecraft.client.renderer.texture.TextureAtlasSprite;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.inventory.InventoryMenu;
import net.minecraft.world.level.material.Fluid;
import net.minecraft.world.level.material.Fluids;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.BackpackWidget;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Dimension;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.GuiHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Position;

import java.util.Optional;

public class FluidFilterControl extends BackpackWidget {
	private final FluidFilterContainer container;

	protected FluidFilterControl(Position position, FluidFilterContainer container) {
		super(position, new Dimension(container.getNumberOfFluidFilters() * 18, 18));
		this.container = container;
	}

	@Override
	protected void renderBg(PoseStack matrixStack, Minecraft minecraft, int mouseX, int mouseY) {
		GuiHelper.renderSlotsBackground(matrixStack, x, y, container.getNumberOfFluidFilters(), 1);
	}

	@Override
	protected void renderWidget(PoseStack matrixStack, int mouseX, int mouseY, float partialTicks) {
		for (int i = 0; i < container.getNumberOfFluidFilters(); i++) {
			Fluid fluid = container.getFluid(i);
			if (fluid != Fluids.EMPTY) {
				ResourceLocation texture = fluid.getAttributes().getStillTexture();
				TextureAtlasSprite still = minecraft.getTextureAtlas(InventoryMenu.BLOCK_ATLAS).apply(texture);
				GuiHelper.renderTiledFluidTextureAtlas(matrixStack, still, fluid.getAttributes().getColor(), x + i * 18 + 1, y + 1, 16);
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
	public void updateNarration(NarrationElementOutput pNarrationElementOutput) {
		//TODO narration
	}
}
