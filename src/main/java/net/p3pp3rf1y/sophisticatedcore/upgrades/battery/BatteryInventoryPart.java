package net.p3pp3rf1y.sophisticatedcore.upgrades.battery;

import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.math.Matrix4f;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TranslatableComponent;
import net.minecraft.world.item.ItemStack;
import net.minecraftforge.fluids.capability.CapabilityFluidHandler;
import net.p3pp3rf1y.sophisticatedcore.SophisticatedCore;
import net.p3pp3rf1y.sophisticatedcore.client.gui.StorageScreen;
import net.p3pp3rf1y.sophisticatedcore.client.gui.UpgradeInventoryPartBase;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.Dimension;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.GuiHelper;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.Position;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.TextureBlitData;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.TranslationHelper;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.UV;
import net.p3pp3rf1y.sophisticatedcore.upgrades.tank.TankClickMessage;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

public class BatteryInventoryPart extends UpgradeInventoryPartBase<BatteryUpgradeContainer> {
	private static final TextureBlitData TANK_BACKGROUND_TOP = new TextureBlitData(GuiHelper.GUI_CONTROLS, Dimension.SQUARE_256, new UV(29, 30), Dimension.SQUARE_18);
	private static final TextureBlitData TANK_BACKGROUND_MIDDLE = new TextureBlitData(GuiHelper.GUI_CONTROLS, Dimension.SQUARE_256, new UV(29, 48), Dimension.SQUARE_18);
	private static final TextureBlitData TANK_BACKGROUND_BOTTOM = new TextureBlitData(GuiHelper.GUI_CONTROLS, Dimension.SQUARE_256, new UV(29, 66), Dimension.SQUARE_18);
	private static final TextureBlitData OVERLAY = new TextureBlitData(GuiHelper.GUI_CONTROLS, Dimension.SQUARE_256, new UV(47, 56), new Dimension(16, 18));
	private static final TextureBlitData CHARGE_SEGMENT = new TextureBlitData(GuiHelper.GUI_CONTROLS, Dimension.SQUARE_256, new UV(47, 74), new Dimension(16, 6));
	private static final TextureBlitData CONNECTION_TOP = new TextureBlitData(GuiHelper.GUI_CONTROLS, Dimension.SQUARE_256, new UV(47, 48), new Dimension(16, 4));
	private static final TextureBlitData CONNECTION_BOTTOM = new TextureBlitData(GuiHelper.GUI_CONTROLS, Dimension.SQUARE_256, new UV(47, 52), new Dimension(16, 4));
	private final Position pos;
	private final int height;
	private final StorageScreen<?> screen;
	private static final int TOP_BAR_COLOR = 0xff1a1a;
	private static final int BOTTOM_BAR_COLOR = 0xffff40;

	public BatteryInventoryPart(int upgradeSlot, BatteryUpgradeContainer container, Position pos, int height, StorageScreen<?> screen) {
		super(upgradeSlot, container);
		this.pos = pos;
		this.height = height;
		this.screen = screen;
	}

	@Override
	public void render(PoseStack poseStack, int mouseX, int mouseY) {
		GuiHelper.blit(poseStack, getTankLeft(), pos.y(), TANK_BACKGROUND_TOP);
		int yOffset = 18;
		for (int i = 0; i < (height - 36) / 18; i++) {
			GuiHelper.blit(poseStack, getTankLeft(), pos.y() + yOffset, TANK_BACKGROUND_MIDDLE);
			yOffset += 18;
		}
		GuiHelper.blit(poseStack, getTankLeft(), pos.y() + yOffset, TANK_BACKGROUND_BOTTOM);

		yOffset = 0;
		for (int i = 0; i < height / 18; i++) {
			GuiHelper.blit(poseStack, getTankLeft() + 1, pos.y() + yOffset, OVERLAY);
			yOffset += 18;
		}

		renderCharge(poseStack);

		GuiHelper.blit(poseStack, getTankLeft() + 1, pos.y(), CONNECTION_TOP);
		GuiHelper.blit(poseStack, getTankLeft() + 1, pos.y() + height - 4, CONNECTION_BOTTOM);
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

		SophisticatedCore.PACKET_HANDLER.sendToServer(new TankClickMessage(upgradeSlot));

		return true;
	}

	@Override
	public void renderErrorOverlay(PoseStack matrixStack) {
		screen.renderOverlay(matrixStack, StorageScreen.ERROR_SLOT_COLOR, getTankLeft() + 1, pos.y() + 1, 16, height - 2);
	}

	@Override
	public void renderTooltip(StorageScreen<?> screen, PoseStack poseStack, int mouseX, int mouseY) {
		int screenX = screen.getGuiLeft() + pos.x() + 10;
		int screenY = screen.getGuiTop() + pos.y() + 1;
		if (mouseX >= screenX && mouseX < screenX + 16 && mouseY >= screenY && mouseY < screenY + height - 2) {
			int energyStored = container.getEnergyStored();
			int maxEnergyStored = container.getMaxEnergyStored();
			List<Component> tooltip = new ArrayList<>();
			tooltip.add(new TranslatableComponent(TranslationHelper.INSTANCE.translUpgradeKey("battery.contents_tooltip"), String.format("%,d", energyStored), String.format("%,d", maxEnergyStored)));
			screen.renderTooltip(poseStack, tooltip, Optional.empty(), mouseX, mouseY);
		}
	}

	private void renderCharge(PoseStack matrixStack) {
		int energyStored = container.getEnergyStored();

		int maxEnergyStored = container.getMaxEnergyStored();

		int segmentHeight = CHARGE_SEGMENT.getHeight();
		int numberOfSegments = height / segmentHeight;
		int displayLevel = (int) (numberOfSegments * ((float) energyStored / maxEnergyStored));

		int finalRed = TOP_BAR_COLOR >> 16 & 255;
		int finalGreen = TOP_BAR_COLOR >> 8 & 255;
		int finalBlue = TOP_BAR_COLOR & 255;

		int initialRed = BOTTOM_BAR_COLOR >> 16 & 255;
		int initialGreen = BOTTOM_BAR_COLOR >> 8 & 255;
		int initialBlue = BOTTOM_BAR_COLOR & 255;

		Matrix4f matrix = matrixStack.last().pose();

		for (int i = 0; i < displayLevel; i++) {
			float percentage = (float) i / (numberOfSegments - 1);
			int red = (int) (initialRed * (1 - percentage) + finalRed * percentage);
			int green = (int) (initialGreen * (1 - percentage) + finalGreen * percentage);
			int blue = (int) (initialBlue * (1 - percentage) + finalBlue * percentage);
			int color = red << 16 | green << 8 | blue | 255 << 24;

			GuiHelper.coloredBlit(matrix, getTankLeft() + 1, pos.y() + height - (i + 1) * segmentHeight, CHARGE_SEGMENT, color);
		}
	}
}
