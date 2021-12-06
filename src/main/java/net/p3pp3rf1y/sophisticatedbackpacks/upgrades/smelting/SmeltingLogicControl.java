package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.smelting;

import com.mojang.blaze3d.vertex.PoseStack;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.narration.NarrationElementOutput;
import net.minecraft.world.inventory.Slot;
import net.minecraft.world.level.Level;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.BackpackWidget;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.CompositeBackpackWidget;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ProgressBar;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Dimension;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.GuiHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Position;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TextureBlitData;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.UV;

import java.util.List;

import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.GuiHelper.GUI_CONTROLS;

public class SmeltingLogicControl extends CompositeBackpackWidget<BackpackWidget> {
	private static final TextureBlitData FURNACE_BACKGROUND = new TextureBlitData(GUI_CONTROLS, Dimension.SQUARE_256, new UV(29, 202), new Dimension(68, 54));
	private static final TextureBlitData COOK_PROGRESS = new TextureBlitData(GUI_CONTROLS, Dimension.SQUARE_256, new UV(100, 239), new Dimension(22, 16));
	private static final TextureBlitData BURN_PROGRESS = new TextureBlitData(GUI_CONTROLS, Dimension.SQUARE_256, new UV(99, 225), new Dimension(14, 14));
	private final SmeltingLogicContainer smeltingLogicContainer;

	public SmeltingLogicControl(Position position, SmeltingLogicContainer smeltingLogicContainer) {
		super(position, new Dimension(68, 54));
		this.smeltingLogicContainer = smeltingLogicContainer;
		addChild(new ProgressBar(new Position(x + 19, y + 18), COOK_PROGRESS, this::getCookProgress, ProgressBar.ProgressDirection.LEFT_RIGHT));
		addChild(new ProgressBar(new Position(x + 1, y + 20), BURN_PROGRESS, this::getBurnProgress, ProgressBar.ProgressDirection.BOTTOM_UP));
	}

	private float getBurnProgress() {
		//noinspection ConstantConditions - world is not null by this point
		return smeltingLogicContainer.isBurning(Minecraft.getInstance().level) ? getProgress(smeltingLogicContainer.getBurnTimeFinish(), smeltingLogicContainer.getBurnTimeTotal()) : 0;
	}

	private float getCookProgress() {
		return smeltingLogicContainer.isCooking() ? getProgress(smeltingLogicContainer.getCookTimeFinish(), smeltingLogicContainer.getCookTimeTotal()) : 0;
	}

	private float getProgress(long finishTime, int timeTotal) {
		Level world = Minecraft.getInstance().level;
		if (world == null) {
			return 0;
		}
		return 1 - ((float) Math.max(finishTime - world.getGameTime(), 0) / timeTotal);
	}

	@Override
	protected void renderBg(PoseStack matrixStack, Minecraft minecraft, int mouseX, int mouseY) {
		GuiHelper.blit(matrixStack, x, y, FURNACE_BACKGROUND);
	}

	public void moveSlotsToView(int screenGuiLeft, int screenGuiTop) {
		List<Slot> smeltingSlots = smeltingLogicContainer.getSmeltingSlots();
		positionSlot(smeltingSlots.get(SmeltingLogic.COOK_INPUT_SLOT), screenGuiLeft, screenGuiTop, 1, 1);
		positionSlot(smeltingSlots.get(SmeltingLogic.COOK_OUTPUT_SLOT), screenGuiLeft, screenGuiTop, 47, 19);
		positionSlot(smeltingSlots.get(SmeltingLogic.FUEL_SLOT), screenGuiLeft, screenGuiTop, 1, 37);
	}

	private void positionSlot(Slot slot, int screenGuiLeft, int screenGuiTop, int xOffset, int yOffset) {
		slot.x = x - screenGuiLeft + xOffset;
		slot.y = y - screenGuiTop + yOffset;
	}

	@Override
	public void updateNarration(NarrationElementOutput pNarrationElementOutput) {
		//TODO add narration
	}
}
