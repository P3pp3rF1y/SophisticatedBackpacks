package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.tank;

import com.mojang.blaze3d.matrix.MatrixStack;
import net.minecraft.client.Minecraft;
import net.minecraft.client.entity.player.ClientPlayerEntity;
import net.minecraft.client.renderer.texture.TextureAtlasSprite;
import net.minecraft.fluid.Fluid;
import net.minecraft.inventory.container.PlayerContainer;
import net.minecraft.item.DyeColor;
import net.minecraft.item.ItemStack;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.text.ITextProperties;
import net.minecraft.util.text.TranslationTextComponent;
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
	public void render(MatrixStack matrixStack, int mouseX, int mouseY) {
		GuiHelper.blit(screen.getMinecraft(), matrixStack, getTankLeft(), pos.getY(), GuiHelper.BAR_BACKGROUND_TOP);
		int yOffset = 18;
		for (int i = 0; i < (height - 36) / 18; i++) {
			GuiHelper.blit(screen.getMinecraft(), matrixStack, getTankLeft(), pos.getY() + yOffset, GuiHelper.BAR_BACKGROUND_MIDDLE);
			yOffset += 18;
		}
		GuiHelper.blit(screen.getMinecraft(), matrixStack, getTankLeft(), pos.getY() + yOffset, GuiHelper.BAR_BACKGROUND_BOTTOM);

		renderFluid(matrixStack, mouseX, mouseY);

		yOffset = 0;
		for (int i = 0; i < height / 18; i++) {
			GuiHelper.blit(screen.getMinecraft(), matrixStack, getTankLeft() + 1, pos.getY() + yOffset, OVERLAY);
			yOffset += 18;
		}
	}

	private int getTankLeft() {
		return pos.getX() + 9;
	}

	@Override
	public boolean handleMouseReleased(double mouseX, double mouseY, int button) {
		if (mouseX < screen.getGuiLeft() + getTankLeft() || mouseX >= screen.getGuiLeft() + getTankLeft() + 18 ||
				mouseY < screen.getGuiTop() + pos.getY() || mouseY >= screen.getGuiTop() + pos.getY() + height) {
			return false;
		}

		ClientPlayerEntity player = screen.getMinecraft().player;
		ItemStack cursorStack = player.inventory.getCarried();
		if (cursorStack.getCount() > 1 || !cursorStack.getCapability(CapabilityFluidHandler.FLUID_HANDLER_ITEM_CAPABILITY).isPresent()) {
			return false;
		}

		PacketHandler.sendToServer(new TankClickMessage(upgradeSlot));

		return true;
	}

	@Override
	public void renderErrorOverlay(MatrixStack matrixStack) {
		screen.renderOverlay(matrixStack, DyeColor.RED.getColorValue() | 0xAA000000, getTankLeft() + 1, pos.getY() + 1, 16, height - 2);
	}

	private void renderTooltip(int mouseX, int mouseY, FluidStack contents, int capacity) {
		int screenX = screen.getGuiLeft() + pos.getX() + 10;
		int screenY = screen.getGuiTop() + pos.getY() + 1;
		if (mouseX >= screenX && mouseX < screenX + 16 && mouseY >= screenY && mouseY < screenY + height - 2) {
			List<ITextProperties> tooltip = new ArrayList<>();
			if (!contents.isEmpty()) {
				tooltip.add(contents.getDisplayName());
			}
			tooltip.add(getContentsTooltip(contents, capacity));
			GuiHelper.setTooltipToRender(tooltip);
		}
	}

	private TranslationTextComponent getContentsTooltip(FluidStack contents, int capacity) {
		if (contents.getFluid().is(ModFluids.EXPERIENCE_TAG)) {
			double contentsLevels = XpHelper.getLevelsForExperience((int) XpHelper.liquidToExperience(contents.getAmount()));
			double tankCapacityLevels = XpHelper.getLevelsForExperience((int) XpHelper.liquidToExperience(capacity));

			return new TranslationTextComponent(TranslationHelper.translUpgradeKey("tank.xp_contents_tooltip"), String.format("%.1f", contentsLevels), String.format("%.1f", tankCapacityLevels));
		}
		return new TranslationTextComponent(TranslationHelper.translUpgradeKey("tank.contents_tooltip"), String.format("%,d", contents.getAmount()), String.format("%,d", capacity));
	}

	private void renderFluid(MatrixStack matrixStack, int mouseX, int mouseY) {
		FluidStack contents = container.getContents();

		int capacity = container.getTankCapacity();
		if (contents.isEmpty()) {
			renderTooltip(mouseX, mouseY, FluidStack.EMPTY, capacity);
			return;
		}

		Fluid fluid = contents.getFluid();
		int fill = contents.getAmount();
		int displayLevel = (int) ((height - 2) * ((float) fill / capacity));

		ResourceLocation texture = fluid.getAttributes().getStillTexture(contents);
		TextureAtlasSprite still = Minecraft.getInstance().getTextureAtlas(PlayerContainer.BLOCK_ATLAS).apply(texture);
		GuiHelper.renderTiledFluidTextureAtlas(matrixStack, still, fluid.getAttributes().getColor(), pos.getX() + 10, pos.getY() + 1 + height - 2 - displayLevel, displayLevel, screen.getMinecraft());
		renderTooltip(mouseX, mouseY, contents, capacity);
	}

}
