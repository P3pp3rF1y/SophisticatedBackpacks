package net.p3pp3rf1y.sophisticatedbackpacks.client.gui;

import com.mojang.blaze3d.matrix.MatrixStack;
import com.mojang.blaze3d.systems.RenderSystem;
import net.minecraft.client.gui.screen.inventory.ContainerScreen;
import net.minecraft.client.renderer.Rectangle2d;
import net.minecraft.entity.player.PlayerInventory;
import net.minecraft.inventory.container.ClickType;
import net.minecraft.inventory.container.Container;
import net.minecraft.inventory.container.Slot;
import net.minecraft.item.ItemStack;
import net.minecraft.util.text.ITextComponent;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.Button;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ButtonDefinitions;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ToggleButton;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.SortBy;

import javax.annotation.Nullable;

import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.GuiHelper.GUI_CONTROLS;

@OnlyIn(Dist.CLIENT)
public class BackpackScreen extends ContainerScreen<BackpackContainer> {
	private static final int UPGRADE_TOP_HEIGHT = 7;
	private static final int UPGRADE_SLOT_HEIGHT = 18;
	private static final int UPGRADE_SPACE_BETWEEN_SLOTS = 4;
	private static final int UPGRADE_BOTTOM_HEIGHT = 7;
	private static final int TOTAL_UPGRADE_GUI_HEIGHT = 252;
	public static final int UPGRADE_INVENTORY_OFFSET = 26;
	private static final int SLOTS_Y_OFFSET = 17;
	private UpgradeSettingsControl upgradeControl;
	private final int slots;
	private Button sortButton;
	private ToggleButton<SortBy> sortByButton;

	public BackpackScreen(BackpackContainer screenContainer, PlayerInventory inv, ITextComponent titleIn) {
		super(screenContainer, inv, titleIn);
		ySize = 114 + getContainer().getNumberOfRows() * 18;
		xSize = getContainer().getBackpackBackgroundProperties().getSlotsOnLine() * 18 + 14;
		playerInventoryTitleY = ySize - 94;
		playerInventoryTitleX = 8 + getContainer().getBackpackBackgroundProperties().getPlayerInventoryXOffset();
		slots = getContainer().getNumberOfUpgradeSlots();
	}

	@Override
	protected void init() {
		super.init();
		initUpgradeControl();
		getContainer().setUpgradeChangeListener(c -> {
			children.remove(upgradeControl);
			initUpgradeControl();
		});
		addSortButtons();
	}

	private void addSortButtons() {
		Position pos = getSortButtonsPosition();

		sortButton = new Button(new Position(pos.getX(), pos.getY()), ButtonDefinitions.SORT, button -> {
			if (button == 0) {
				getContainer().sort();
			}
		});
		addListener(sortButton);
		sortByButton = new ToggleButton<>(new Position(pos.getX() + 14, pos.getY()), ButtonDefinitions.SORT_BY, button -> {
			if (button == 0) {
				getContainer().setSortBy(getContainer().getSortBy().next());
			}
		}, () -> getContainer().getSortBy());
		addListener(sortByButton);
	}

	private Position getSortButtonsPosition() {
		switch (Config.CLIENT.sortButtonsPosition.get()) {
			case ABOVE_UPGRADES:
				return new Position(guiLeft - UPGRADE_INVENTORY_OFFSET - 2, guiTop + getUpgradeTop() - 14);
			case BELOW_UPGRADES:
				return new Position(guiLeft - UPGRADE_INVENTORY_OFFSET - 2, guiTop + getUpgradeTop() + getUpgradeHeightWithoutBottom() + UPGRADE_BOTTOM_HEIGHT + 2);
			case BELOW_UPGRADE_TABS:
				return upgradeControl == null ? new Position(0, 0) : new Position(upgradeControl.getX() + 2, upgradeControl.getY() + Math.max(0, upgradeControl.getHeight() + 2));
			case TITLE_LINE_RIGHT:
			default:
				return new Position(guiLeft + xSize - 34, guiTop + 4);
		}
	}

	public Rectangle2d getSortButtonsRectangle() {
		return new Rectangle2d(sortButton.getX(), sortButton.getY(), sortByButton.getX() + sortByButton.getWidth() - sortButton.getX(),
				sortByButton.getY() + sortByButton.getHeight() - sortButton.getY());
	}

	private void initUpgradeControl() {
		upgradeControl = new UpgradeSettingsControl(new Position(guiLeft + xSize, guiTop + 4), this);
		addListener(upgradeControl);
	}

	@Override
	public void render(MatrixStack matrixStack, int mouseX, int mouseY, float partialTicks) {
		renderBackground(matrixStack);
		upgradeControl.render(matrixStack, mouseX, mouseY, partialTicks);
		matrixStack.translate(0, 0, 200);
		super.render(matrixStack, mouseX, mouseY, partialTicks);
		sortButton.render(matrixStack, mouseX, mouseY, partialTicks);
		sortByButton.render(matrixStack, mouseX, mouseY, partialTicks);
		renderHoveredTooltip(matrixStack, mouseX, mouseY);
	}

	@Override
	protected void drawGuiContainerForegroundLayer(MatrixStack matrixStack, int mouseX, int mouseY) {
		super.drawGuiContainerForegroundLayer(matrixStack, mouseX, mouseY);
		renderUpgradeSlots(matrixStack, mouseX, mouseY);
	}

	private void renderUpgradeSlots(MatrixStack matrixStack, int mouseX, int mouseY) {
		for (int slotId = 0; slotId < container.upgradeSlots.size(); ++slotId) {
			Slot slot = container.upgradeSlots.get(slotId);
			if (slot.isEnabled()) {
				moveItems(matrixStack, slot);
			}

			if (isSlotSelected(slot, mouseX, mouseY) && slot.isEnabled()) {
				hoveredSlot = slot;
				RenderSystem.disableDepthTest();
				int xPos = slot.xPos;
				int yPos = slot.yPos;
				RenderSystem.colorMask(true, true, true, false);
				int slotColor = getSlotColor(slotId);
				fillGradient(matrixStack, xPos, yPos, xPos + 16, yPos + 16, slotColor, slotColor);
				RenderSystem.colorMask(true, true, true, true);
				RenderSystem.enableDepthTest();
			}
		}
	}

	protected void drawGuiContainerBackgroundLayer(MatrixStack matrixStack, float partialTicks, int x, int y) {
		drawInventoryBackground(matrixStack);
		drawUpgradeBackground(matrixStack);
	}

	@Override
	protected void renderHoveredTooltip(MatrixStack matrixStack, int x, int y) {
		super.renderHoveredTooltip(matrixStack, x, y);
		GuiHelper.renderToolTip(minecraft, matrixStack, x, y);
	}

	private void drawInventoryBackground(MatrixStack matrixStack) {
		RenderSystem.color4f(1.0F, 1.0F, 1.0F, 1.0F);
		minecraft.getTextureManager().bindTexture(getContainer().getBackpackBackgroundProperties().getTextureName());
		int x = (width - xSize) / 2;
		int y = (height - ySize) / 2;
		int inventorySlots = getContainer().getBackpackInventorySlots().size();
		int slotsOnLine = getContainer().getBackpackBackgroundProperties().getSlotsOnLine();
		int slotRows = inventorySlots / slotsOnLine;
		int remainingSlots = inventorySlots % slotsOnLine;
		int slotsHeight = 18 * (slotRows + (remainingSlots > 0 ? 1 : 0));
		int halfSlotHeight = slotsHeight / 2;
		blit(matrixStack, x, y, 0, 0, xSize, SLOTS_Y_OFFSET + halfSlotHeight, 256, 256);
		int playerInventoryHeight = 97;
		blit(matrixStack, x, y + SLOTS_Y_OFFSET + halfSlotHeight, 0, (float) 256 - (playerInventoryHeight + halfSlotHeight), xSize, playerInventoryHeight + halfSlotHeight, 256, 256);

		GuiHelper.renderSlotsBackground(minecraft, matrixStack, x + 7, y + SLOTS_Y_OFFSET, slotsOnLine, slotRows, remainingSlots);
	}

	private void drawUpgradeBackground(MatrixStack matrixStack) {
		RenderSystem.color4f(1.0F, 1.0F, 1.0F, 1.0F);
		minecraft.getTextureManager().bindTexture(GUI_CONTROLS);
		int i = (width - xSize) / 2;
		int j = (height - ySize) / 2;

		int firstHalfHeight = getUpgradeHeightWithoutBottom();

		blit(matrixStack, i - UPGRADE_INVENTORY_OFFSET, j + getUpgradeTop(), 0, 0, 29, firstHalfHeight, 256, 256);
		blit(matrixStack, i - UPGRADE_INVENTORY_OFFSET, j + getUpgradeTop() + firstHalfHeight, 0, (float) TOTAL_UPGRADE_GUI_HEIGHT - UPGRADE_BOTTOM_HEIGHT, 29, UPGRADE_BOTTOM_HEIGHT, 256, 256);
	}

	public int getUpgradeTop() {
		return ySize - 94 - getUpgradeHeight();
	}

	public int getUpgradeHeight() {
		return getUpgradeHeightWithoutBottom() + UPGRADE_TOP_HEIGHT;
	}

	private int getUpgradeHeightWithoutBottom() {
		return UPGRADE_BOTTOM_HEIGHT + slots * UPGRADE_SLOT_HEIGHT + (slots - 1) * UPGRADE_SPACE_BETWEEN_SLOTS;
	}

	public UpgradeSettingsControl getUpgradeControl() {
		if (upgradeControl == null) {
			upgradeControl = new UpgradeSettingsControl(new Position(guiLeft + xSize, guiTop + 4), this);
		}
		return upgradeControl;
	}

	@Nullable
	@Override
	protected Slot getSelectedSlot(double mouseX, double mouseY) {
		for (int i = 0; i < container.upgradeSlots.size(); ++i) {
			Slot slot = container.upgradeSlots.get(i);
			if (isSlotSelected(slot, mouseX, mouseY) && slot.isEnabled()) {
				return slot;
			}
		}

		return super.getSelectedSlot(mouseX, mouseY);
	}

	@Override
	public boolean mouseReleased(double mouseX, double mouseY, int button) {
		Slot slot = getSelectedSlot(mouseX, mouseY);

		if (doubleClick && slot != null && button == 0 && container.canMergeSlot(ItemStack.EMPTY, slot) && hasShiftDown() && !shiftClickedSlot.isEmpty()) {
			for (Slot slot2 : container.upgradeSlots) {
				if (slot2 != null && slot2.canTakeStack(minecraft.player) && slot2.getHasStack() && slot2.isSameInventory(slot) && Container.canAddItemToSlot(slot2, shiftClickedSlot, true)) {
					handleMouseClick(slot2, slot2.slotNumber, button, ClickType.QUICK_MOVE);
				}
			}
		}

		return super.mouseReleased(mouseX, mouseY, button);
	}

	@Override
	protected boolean hasClickedOutside(double mouseX, double mouseY, int guiLeftIn, int guiTopIn, int mouseButton) {
		return super.hasClickedOutside(mouseX, mouseY, guiLeftIn, guiTopIn, mouseButton) && hasClickedOutsideOfUpgradeSlots(mouseX, mouseY)
				&& hasClickedOutsideOfUpgradeSettings(mouseX, mouseY);
	}

	private boolean hasClickedOutsideOfUpgradeSettings(double mouseX, double mouseY) {
		return upgradeControl.getTabRectangles().stream().noneMatch(r -> r.contains((int) mouseX, (int) mouseY));
	}

	private boolean hasClickedOutsideOfUpgradeSlots(double mouseX, double mouseY) {
		return !getUpgradeSlotsRectangle().contains((int) mouseX, (int) mouseY);
	}

	public Rectangle2d getUpgradeSlotsRectangle() {
		return new Rectangle2d(getGuiLeft() - BackpackScreen.UPGRADE_INVENTORY_OFFSET, getGuiTop() + getUpgradeTop(), 32, getUpgradeHeight());
	}
}
