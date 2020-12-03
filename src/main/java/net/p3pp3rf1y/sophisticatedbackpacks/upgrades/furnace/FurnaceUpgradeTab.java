package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.furnace;

import com.mojang.blaze3d.matrix.MatrixStack;
import net.minecraft.client.Minecraft;
import net.minecraft.inventory.container.Slot;
import net.minecraft.util.text.TranslationTextComponent;
import net.minecraft.world.World;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.Dimension;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.GuiHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.Position;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.TextureBlitData;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.UV;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.UpgradeSettingsTab;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ProgressBar;

import java.util.List;

import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.GuiHelper.UPGRADE_CONTROLS;
import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.TranslationHelper.translUpgrade;
import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.TranslationHelper.translUpgradeTooltip;

@OnlyIn(Dist.CLIENT)
public class FurnaceUpgradeTab extends UpgradeSettingsTab<FurnaceUpgradeContainer> {
	private static final TextureBlitData FURNACE_BACKGROUND = new TextureBlitData(UPGRADE_CONTROLS, Dimension.SQUARE_256, new UV(29, 202), new Dimension(68, 54));
	private static final TextureBlitData COOK_PROGRESS = new TextureBlitData(UPGRADE_CONTROLS, Dimension.SQUARE_256, new UV(100, 239), new Dimension(22, 16));
	private static final TextureBlitData BURN_PROGRESS = new TextureBlitData(UPGRADE_CONTROLS, Dimension.SQUARE_256, new UV(99, 225), new Dimension(14, 14));

	public FurnaceUpgradeTab(FurnaceUpgradeContainer upgradeContainer, Position position, BackpackScreen screen) {
		super(upgradeContainer, position, new Dimension(78, 85), screen, new TranslationTextComponent(translUpgrade("furnace")),
				new TranslationTextComponent(translUpgradeTooltip("furnace")));
		addHideableChild(new ProgressBar(new Position(x + 22, y + 42), COOK_PROGRESS, this::getCookProgress, ProgressBar.ProgressDirection.LEFT_RIGHT));
		addHideableChild(new ProgressBar(new Position(x + 4, y + 44), BURN_PROGRESS, this::getBurnProgress, ProgressBar.ProgressDirection.BOTTOM_UP));
	}

	private float getBurnProgress() {
		//noinspection ConstantConditions - world is not null by this point
		return getContainer().isBurning(Minecraft.getInstance().world) ? getProgress(getContainer().getBurnTimeFinish(), getContainer().getBurnTimeTotal()) : 0;
	}

	private float getCookProgress() {
		return getContainer().isCooking() ? getProgress(getContainer().getCookTimeFinish(), getContainer().getCookTimeTotal()) : 0;
	}

	private float getProgress(long finishTime, int timeTotal) {
		World world = Minecraft.getInstance().world;
		if (world == null) {
			return 0;
		}
		return 1 - ((float) Math.max(finishTime - world.getGameTime(), 0) / timeTotal);
	}

	@Override
	protected void moveSlotsToTab() {
		List<Slot> slots = getContainer().getSlots();
		positionSlot(slots.get(FurnaceUpgradeWrapper.COOK_INPUT_SLOT), 4, 25);
		positionSlot(slots.get(FurnaceUpgradeWrapper.COOK_OUTPUT_SLOT), 50, 43);
		positionSlot(slots.get(FurnaceUpgradeWrapper.FUEL_SLOT), 4, 61);
	}

	private void positionSlot(Slot slot, int xOffset, int yOffset) {
		slot.xPos = x - screen.getGuiLeft() + xOffset;
		slot.yPos = y - screen.getGuiTop() + yOffset;
	}

	@Override
	protected void renderBg(MatrixStack matrixStack, Minecraft minecraft, int mouseX, int mouseY) {
		super.renderBg(matrixStack, minecraft, mouseX, mouseY);
		if (isOpen) {
			GuiHelper.blit(minecraft, matrixStack, x + 3, y + 24, FURNACE_BACKGROUND);
		}
	}
}
