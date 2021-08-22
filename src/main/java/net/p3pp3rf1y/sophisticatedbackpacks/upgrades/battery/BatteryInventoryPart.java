package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.battery;

import com.mojang.blaze3d.matrix.MatrixStack;
import net.minecraft.client.entity.player.ClientPlayerEntity;
import net.minecraft.item.DyeColor;
import net.minecraft.item.ItemStack;
import net.minecraft.util.math.vector.Matrix4f;
import net.minecraft.util.text.ITextProperties;
import net.minecraft.util.text.TranslationTextComponent;
import net.minecraftforge.fluids.capability.CapabilityFluidHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.UpgradeInventoryPartBase;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Dimension;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.GuiHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Position;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TextureBlitData;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TranslationHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.UV;
import net.p3pp3rf1y.sophisticatedbackpacks.network.PacketHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.tank.TankClickMessage;

import java.util.ArrayList;
import java.util.List;

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
	private final BackpackScreen screen;
	private static final int TOP_BAR_COLOR = 0xff1a1a;
	private static final int BOTTOM_BAR_COLOR = 0xffff40;

	public BatteryInventoryPart(int upgradeSlot, BatteryUpgradeContainer container, Position pos, int height, BackpackScreen screen) {
		super(upgradeSlot, container);
		this.pos = pos;
		this.height = height;
		this.screen = screen;
	}

	@Override
	public void render(MatrixStack matrixStack, int mouseX, int mouseY) {
		GuiHelper.blit(screen.getMinecraft(), matrixStack, getTankLeft(), pos.getY(), TANK_BACKGROUND_TOP);
		int yOffset = 18;
		for (int i = 0; i < (height - 36) / 18; i++) {
			GuiHelper.blit(screen.getMinecraft(), matrixStack, getTankLeft(), pos.getY() + yOffset, TANK_BACKGROUND_MIDDLE);
			yOffset += 18;
		}
		GuiHelper.blit(screen.getMinecraft(), matrixStack, getTankLeft(), pos.getY() + yOffset, TANK_BACKGROUND_BOTTOM);

		yOffset = 0;
		for (int i = 0; i < height / 18; i++) {
			GuiHelper.blit(screen.getMinecraft(), matrixStack, getTankLeft() + 1, pos.getY() + yOffset, OVERLAY);
			yOffset += 18;
		}

		renderCharge(matrixStack, mouseX, mouseY);

		GuiHelper.blit(screen.getMinecraft(), matrixStack, getTankLeft() + 1, pos.getY(), CONNECTION_TOP);
		GuiHelper.blit(screen.getMinecraft(), matrixStack, getTankLeft() + 1, pos.getY() + height - 4, CONNECTION_BOTTOM);
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
		if (!cursorStack.getCapability(CapabilityFluidHandler.FLUID_HANDLER_ITEM_CAPABILITY).isPresent()) {
			return false;
		}

		PacketHandler.sendToServer(new TankClickMessage(upgradeSlot));

		return true;
	}

	@Override
	public void renderErrorOverlay(MatrixStack matrixStack) {
		screen.renderOverlay(matrixStack, DyeColor.RED.getColorValue() | 0xAA000000, getTankLeft() + 1, pos.getY() + 1, 16, height - 2);
	}

	private void renderTooltip(int mouseX, int mouseY, int energyStored, int maxEnergyStored) {
		int screenX = screen.getGuiLeft() + pos.getX() + 10;
		int screenY = screen.getGuiTop() + pos.getY() + 1;
		if (mouseX >= screenX && mouseX < screenX + 16 && mouseY >= screenY && mouseY < screenY + height - 2) {
			List<ITextProperties> tooltip = new ArrayList<>();
			tooltip.add(new TranslationTextComponent(TranslationHelper.translUpgradeKey("battery.contents_tooltip"), String.format("%,d", energyStored), String.format("%,d", maxEnergyStored)));
			GuiHelper.setTooltipToRender(tooltip);
		}
	}

	private void renderCharge(MatrixStack matrixStack, int mouseX, int mouseY) {
		int energyStored = container.getEnergyStored();

		int maxEneergyStored = container.getMaxEnergyStored();

		int segmentHeight = CHARGE_SEGMENT.getHeight();
		int numberOfSegments = height / segmentHeight;
		int displayLevel = (int) (numberOfSegments * ((float) energyStored / maxEneergyStored));

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

			GuiHelper.coloredBlit(matrix, getTankLeft() + 1, pos.getY() + height - (i + 1) * segmentHeight, CHARGE_SEGMENT, color);
		}

		renderTooltip(mouseX, mouseY, energyStored, maxEneergyStored);
	}
}
