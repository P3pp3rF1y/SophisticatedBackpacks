package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.tank;

import com.mojang.blaze3d.vertex.PoseStack;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.texture.TextureAtlasSprite;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TranslatableComponent;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.inventory.InventoryMenu;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.material.Fluid;
import net.minecraftforge.fluids.FluidStack;
import net.minecraftforge.fluids.capability.CapabilityFluidHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.UpgradeInventoryPartBase;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Dimension;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.GuiHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Position;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TextureBlitData;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TranslationHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.UV;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModFluids;
import net.p3pp3rf1y.sophisticatedbackpacks.network.PacketHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.util.XpHelper;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

public class TankInventoryPart extends UpgradeInventoryPartBase<TankUpgradeContainer> {
	private static final TextureBlitData OVERLAY = new TextureBlitData(GuiHelper.GUI_CONTROLS, Dimension.SQUARE_256, new UV(47, 30), new Dimension(16, 18));
	private final Position pos;
	private final int height;
	private final BackpackScreen screen;

	public TankInventoryPart(int upgradeSlot, TankUpgradeContainer container, Position pos, int height, BackpackScreen screen) {
		super(upgradeSlot, container);
		this.pos = pos;
		this.height = height;
		this.screen = screen;
	}

	@Override
	public void render(PoseStack matrixStack, int mouseX, int mouseY) {
		GuiHelper.blit(matrixStack, getTankLeft(), pos.y(), GuiHelper.BAR_BACKGROUND_TOP);
		int yOffset = 18;
		for (int i = 0; i < (height - 36) / 18; i++) {
			GuiHelper.blit(matrixStack, getTankLeft(), pos.y() + yOffset, GuiHelper.BAR_BACKGROUND_MIDDLE);
			yOffset += 18;
		}
		GuiHelper.blit(matrixStack, getTankLeft(), pos.y() + yOffset, GuiHelper.BAR_BACKGROUND_BOTTOM);

		renderFluid(matrixStack);

		yOffset = 0;
		for (int i = 0; i < height / 18; i++) {
			GuiHelper.blit(matrixStack, getTankLeft() + 1, pos.y() + yOffset, OVERLAY);
			yOffset += 18;
		}
	}

	private int getTankLeft() {
		return pos.x() + 9;
	}

	@Override
	public boolean handleMouseReleased(double mouseX, double mouseY, int button) {
		if (mouseX < screen.getGuiLeft() + getTankLeft() || mouseX >= screen.getGuiLeft() + getTankLeft() + 18 ||
				mouseY < screen.getGuiTop() + pos.y() || mouseY >= screen.getGuiTop() + pos.y() + height) {
			return false;
		}

		ItemStack cursorStack = screen.getMenu().getCarried();
		if (!cursorStack.getCapability(CapabilityFluidHandler.FLUID_HANDLER_ITEM_CAPABILITY).isPresent()) {
			return false;
		}

		PacketHandler.sendToServer(new TankClickMessage(upgradeSlot));

		return true;
	}

	@Override
	public void renderErrorOverlay(PoseStack matrixStack) {
		screen.renderOverlay(matrixStack, BackpackScreen.ERROR_SLOT_COLOR, getTankLeft() + 1, pos.y() + 1, 16, height - 2);
	}

	@Override
	public void renderTooltip(BackpackScreen screen, PoseStack poseStack, int mouseX, int mouseY) {
		FluidStack contents = container.getContents();
		int capacity = container.getTankCapacity();
		if (contents.isEmpty()) {
			contents = FluidStack.EMPTY;
		}

		int screenX = screen.getGuiLeft() + pos.x() + 10;
		int screenY = screen.getGuiTop() + pos.y() + 1;
		if (mouseX >= screenX && mouseX < screenX + 16 && mouseY >= screenY && mouseY < screenY + height - 2) {
			List<Component> tooltip = new ArrayList<>();
			if (!contents.isEmpty()) {
				tooltip.add(contents.getDisplayName());
			}
			tooltip.add(getContentsTooltip(contents, capacity));
			screen.renderTooltip(poseStack, tooltip, Optional.empty(), mouseX, mouseY);
		}
	}

	private TranslatableComponent getContentsTooltip(FluidStack contents, int capacity) {
		if (contents.getFluid().is(ModFluids.EXPERIENCE_TAG)) {
			double contentsLevels = XpHelper.getLevelsForExperience((int) XpHelper.liquidToExperience(contents.getAmount()));
			double tankCapacityLevels = XpHelper.getLevelsForExperience((int) XpHelper.liquidToExperience(capacity));

			return new TranslatableComponent(TranslationHelper.translUpgradeKey("tank.xp_contents_tooltip"), String.format("%.1f", contentsLevels), String.format("%.1f", tankCapacityLevels));
		}
		return new TranslatableComponent(TranslationHelper.translUpgradeKey("tank.contents_tooltip"), String.format("%,d", contents.getAmount()), String.format("%,d", capacity));
	}

	private void renderFluid(PoseStack matrixStack) {
		FluidStack contents = container.getContents();
		int capacity = container.getTankCapacity();
		if (contents.isEmpty()) {
			return;
		}

		Fluid fluid = contents.getFluid();
		int fill = contents.getAmount();
		int displayLevel = (int) ((height - 2) * ((float) fill / capacity));

		ResourceLocation texture = fluid.getAttributes().getStillTexture(contents);
		TextureAtlasSprite still = Minecraft.getInstance().getTextureAtlas(InventoryMenu.BLOCK_ATLAS).apply(texture);
		GuiHelper.renderTiledFluidTextureAtlas(matrixStack, still, fluid.getAttributes().getColor(), pos.x() + 10, pos.y() + 1 + height - 2 - displayLevel, displayLevel);
	}

}
